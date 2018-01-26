# Walking School Bus start file

# Idea: Build a simulation study that ogranically creates data points,
#       Calls Google API in path mapping, and optimizes number of walking school busses
#       With minimum requirements on student distance and number of busses

# Step 1: Grab a set of schools and their corresponding catchment regions
library(sp)
library(tidyverse)
library(rvest)
library(pdftools)
library(rvest)
library(ggmap)
library(rgdal)

# Something to consider if we pursue this project

# # Get link URLs
# main.page <- read_html(x = "http://www.sd41.bc.ca/elementary-school-addresses/")
# urls <- main.page %>% html_nodes("td a") %>% html_attr("href")
# 
# # Get the map pdfs
# pdfList <- list()
# for(i in 1:length(urls)){
#   pdfList[i] <- download.file(urls[i], paste0("Map", i, ".pdf"), mode = "wb")
# }
# 
# # View the images
# txt <- pdf_text("Map7.pdf")

# Use an arbitrary school as a base graph
ArmstrongSchool <- get_map(location = c(lon = -122.9083027 ,lat = 49.2339746), zoom = 14)
plottedSchool <- ggmap(ArmstrongSchool) + geom_point(aes(x = -122.9083027, y = 49.2339746, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none")

# Create a polygon bounding the school catchment
  # Will be done heuristically for now
lon <- c(-122.917433, -122.926703, -122.927647, -122.930436, -122.927475, -122.931520, -122.914654, -122.913066, -122.906414, -122.892510, -122.892682, -122.897660)
lat <- c(49.225133, 49.232405, 49.231957, 49.234297, 49.235838, 49.239243, 49.249329, 49.247760, 49.247087, 49.240587, 49.235656, 49.235124)
polyCoords <- as.data.frame(cbind(lon, lat)) 
rownames(polyCoords) = letters[1:dim(polyCoords)[1]]

# Update graph with catchment
plottedSchool <- plottedSchool + geom_polygon(data = polyCoords, aes(x = lon, y = lat) , alpha = 0.3, colour = "red", fill = "red") 
  
# Create a bounding box that I can sample from
coords <- cbind(lon, lat)
P1 <- Polygon(coords)
Ps1 <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # This creates the desired polygon boundary

# Create a bouding box for the residential area only for more reasonable point generation
lonRes <- c(-122.917433, -122.926703, -122.908289, -122.892603, -122.892682, -122.897660)
latRes <- c(49.225133, 49.232405, 49.242508, 49.237703, 49.235656, 49.235124)
coordsRes <- cbind(lonRes, latRes)
P2 <- Polygon(coordsRes)
Ps2 <- SpatialPolygons(list(Polygons(list(P2), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # This creates the desired polygon boundary

# Sample n students from the region and add to the plot. Raw data will be used again later for API call
n = 10
randomHouses <- spsample(Ps2, n = n, type = "random")@coords %>% as.data.frame() # Use Ps2 as this is a more realistic area for people to be distributed. The boundary for the region still holds, however
studentsInCatchment <- plottedSchool + geom_point(data = randomHouses, aes(x = x, y = y))
studentsInCatchment

# Select some of the random houses to represent parents that are willing to lead the WSB
  # Do this according to diff probability distributions
    # Uniform
    uniVals <- runif(n) #[0, 1]
    
    # Normal
    normVals <- rnorm(n) #(-inf, inf), mean = 0, sd = 1
    
    # Log Normal
    logNVals <- rlnorm(n) # (0, inf), mean = exp^.5 = 1.61
    
# Create a systematic way to decide which houses are volunteer leaders    
houses <- cbind(randomHouses, uniVals, normVals, logNVals)    
houses$UniLeader <- (houses$uniVals > 0.5)
houses$normLeader <- (houses$normVals > 0.5)
houses$logNLeader <- (houses$logNVals > 1.61)
head(houses)
studentsAndLeaders <- plottedSchool + geom_point(data = houses, aes(x = x, y=y, col = logNLeader))
studentsAndLeaders

# Begin building the API scaffolding
  # First need to cluster points about the leaders that are closest to them so that the API call can handle number of observations
  # Use KNN as we get fixed start points based on the below train/test method
  library(class)
  centers <- houses %>% filter(logNLeader == TRUE)
  cl <- as.factor(1:length(centers$x))
  clusters <- knn(train = centers[, 1:2], test = houses[, 1:2], cl = cl, k = 1)
  housesClustered <- houses %>% add_column(as.factor(clusters))
  clusterPlot <- plottedSchool + geom_point(data = housesClustered, aes(x = x, y = y, col = factor(clusters), size = 4, shape = logNLeader))
  clusterPlot  
  
  
  
# Testing the file Neggyn found on the data custodian, url = http://data.vancouver.ca/datacatalogue/publicPlaces.htm
library(jsonlite)
schoolBoundaries <- fromJSON("elementary_school_boundaries.json")
View(schoolBoundaries)

# x and y coordinates are separated weirdly in the read data
# x are approx 49000, y are approx 54000. Since coord pairs, mid point is the split I want. Can be achieved by a triple index
  #schoolBoundaries$features$geometry$coordinates[[1]][, , 1]
  #schoolBoundaries$features$geometry$coordinates[[1]][, , 2]



######## Testing stuff for conversion of coordinate systems: Status = Working #########
# If the function breaks visit: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf

# Create a function that takes the jsonData and a given school and outputs its catchment map and bounding polygon. Will throw error for invalid school at the moment
schoolCatch <- function(jsonData, schoolName){
  index <- which(jsonData$features$properties$NAME == schoolName) # Select the school set of coords 
  Easting <- jsonData$features$geometry$coordinates[[index]][, , 1] # Separate to x and y
  Northing<- jsonData$features$geometry$coordinates[[index]][, , 2]
  ID <- data.frame(1:length(Easting)) # ID var for later
  coords <- cbind(Easting, Northing) %>% as.data.frame()
  catch_SP <- SpatialPointsDataFrame(coords, data = ID, proj4string = CRS("+init=EPSG:26910")) # init = EPSG:26910 is a region specific identifier for coord system
  catch_LL <- spTransform(catch_SP, CRS("+init=epsg:4326")) # This one is general for long-lat conversions
  final_DF <- as.data.frame(catch_LL@coords)
  names(final_DF) <- c("Longitude", "Latitude")
  return(final_DF) # Return the now long-lat coordinates
}

# Example usage where testPlot is already the map centered at Charles Dickens
gmap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14, maptype = "hybrid")
testPlot <- ggmap(gmap) + geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none")


charlesDickensTest <- schoolCatch(schoolBoundaries, "Charles Dickens Elementary")
charlesDickensTest
testCharles <- testPlot + geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red")

# Generate the sample data of students for the region
P3 <- Polygon(charlesDickensTest)
Ps3 <- SpatialPolygons(list(Polygons(list(P3), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # This creates the desired polygon boundary
m = 15
randomHousesNew <- spsample(Ps3, n = m, type = "random")@coords %>% as.data.frame() # Use Ps2 as this is a more realistic area for people to be distributed. The boundary for the region still holds, however

# Plot it all together
studentsInCharles <- testCharles + geom_point(data = randomHousesNew, aes(x = x, y = y))
studentsInCharles
