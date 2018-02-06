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
library(class)
library(stringr)

# Something to consider if we pursue this project

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
  centers <- houses %>% filter(logNLeader == TRUE)
  cl <- as.factor(1:length(centers$x))
  clusters <- knn(train = centers[, 1:2], test = houses[, 1:2], cl = cl, k = 1)
  housesClustered <- houses %>% add_column(as.factor(clusters))
  clusterPlot <- plottedSchool + geom_point(data = housesClustered, aes(x = x, y = y, col = factor(clusters), size = 3, shape = logNLeader))
  clusterPlot  
  
  
  
# Testing the file Neggyn found on the data custodian, url = http://data.vancouver.ca/datacatalogue/publicPlaces.htm
library(jsonlite)
schoolBoundaries <- fromJSON("elementary_school_boundaries.json")


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

# Function to create polygon boundaries, takes the result of schoolCatch and a sample size as args and outputs sampled points
polygons <- function(n, catchLongLat){
  P <- Polygon(catchLongLat)
  Ps <- SpatialPolygons(list(Polygons(list(P), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  houses <- spsample(Ps, n = n, type = "random")@coords %>% as.data.frame()
  return(houses)
}

# Example usage where testPlot is already the map centered at Charles Dickens
gmap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14, maptype = "hybrid")
testPlot <- ggmap(gmap) + geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none")


charlesDickensTest <- schoolCatch(schoolBoundaries, "Charles Dickens Elementary")
charlesDickensTest
testCharles <- testPlot + geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red")

# Generate the sample data of students for the region
charlesSample <- polygons(15, charlesDickensTest)

# Plot it all together
studentsInCharles <- testCharles + geom_point(data = charlesSample, aes(x = x, y = y))
studentsInCharles

# Function to assign leaders to the sampled points, just using uniform dist at this point
leaders <- function(sampledPoints){
  nPoints <- dim(sampledPoints)[1]
  uniPoints <- runif(nPoints)
  leader <- (uniPoints > 0.65)
  leaderPoints <- cbind(sampledPoints, leader) %>% as.data.frame()
  return(leaderPoints)
}



# Need clustering function to assign points to a leader. Currently uses knn and returns coords, leaders, and cluster membership
groups <- function(nodesAndLeaders){
  centers <- nodesAndLeaders %>% filter(leader == TRUE)
  cl <- as.factor(1:length(centers$x))
  clusters <- knn(train = centers[, 1:2], test = nodesAndLeaders[, 1:2], cl = cl, k = 1) %>% as.factor
  housesClustered <- nodesAndLeaders %>% add_column(clusters)
  return(housesClustered)
}

# Standard procedure to generate necessary info: Define school boundaries from file, sample points from polygon, assign leaders, group walkers, plot output
charlesDickensTest <- schoolCatch(schoolBoundaries, "Charles Dickens Elementary")
charlesSample <- polygons(15, charlesDickensTest)
lead <- leaders(charlesSample)
clustered <- groups(lead)

schoolMap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14, maptype = "hybrid")
schoolMapWithPoints <- ggmap(schoolMap) + 
                       geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
                       geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
                       geom_point(data = clustered, aes(x = x, y = y, col = clusters, shape = leader))

############################################
##### Google routing distance API work #####
############################################

googleKey <- "AIzaSyDNSh3L8O1buhgauPwqqj6_Hb14HzgCXcg"

# Takes Lat, Long style coordinates
googleAPICaller <- function(key, origin, waypoints, dest){
  if(length(waypoints) > 0){
    originToSchool <- paste0("https://maps.googleapis.com/maps/api/directions/json?units=metric&origin=", origin, "&destination=", dest,
                             "&waypoints=", waypoints, "&mode=walking&key=", key)
  }
  else{
    originToSchool <- paste0("https://maps.googleapis.com/maps/api/directions/json?units=metric&origin=", origin, "&destination=", dest,
                             "&mode=walking&key=", key)
  }
  print(originToSchool)
  holder <- fromJSON(originToSchool)
  return(holder)
}


# Function that takes the starting data frame and creates route string for API call. Should have the leader as starting node
routeCreator <- function(clusterDF){
  leaders <- clusterDF %>% filter(leader == TRUE)
  nodes <- clusterDF %>% filter(leader != TRUE)
  textData <- nodes %>% mutate(combo = paste0(.$y, ",", .$x))
  routeList <- split(textData, textData$clusters)
  routes <- list()
  result <- list()
  for(i in 1:length(names(routeList))){
    temp <- with(leaders[i,], paste0(y, ",", x))
    result[[i]] <- list(temp, with(routeList[[i]], paste0(combo)) %>% str_c(collapse = "|"))
  }
  return(result)
}
testSet <- routeCreator(clustered)
testSet

################ Test Google Call for Charles Dickens and simulated data ##############

charlesDickensTest <- schoolCatch(schoolBoundaries, "Charles Dickens Elementary")
charlesSample <- polygons(15, charlesDickensTest)
lead <- leaders(charlesSample)
clustered <- groups(lead)
charlesRoutes <- routeCreator(clustered)
charlesDickensLocation <- "49.254957,-123.083038"

# Test call for the 4th route: Key, origin, waypoints, destination
googleCall <- googleAPICaller(googleKey, testSet[[4]][[1]], testSet[[4]][[2]], charlesDickensLocation)



# Graph yet to be integrated with the path
schoolMap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14, maptype = "hybrid")
schoolMapWithPoints <- ggmap(schoolMap) + 
  geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
  geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
  geom_point(data = clustered, aes(x = x, y = y, col = clusters, shape = leader))
