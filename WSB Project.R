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
coords = cbind(lon, lat)
P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) # This creates the desired polygon boundary

# Sample n students from the region and add to the plot. Raw data will be used again later for API call
n = 10
randomHouses <- spsample(Ps1, n = n, type = "random")@coords %>% as.data.frame()
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
