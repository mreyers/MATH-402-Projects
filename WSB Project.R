# Walking School Bus start file

# Idea: Build a simulation study that ogranically creates data points,
#       Calls Google API in path mapping, and optimizes number of walking school busses
#       With minimum requirements on student distance and number of busses

# Step 1: Libraries
library(sp)
library(tidyverse)
library(rvest)
library(pdftools)
library(rvest)
library(ggmap)
library(rgdal)
library(class)
library(stringr)
library(jsonlite)


# Step 2: Grab a set of schools and their corresponding catchment regions  
# Testing the file Neggyn found on the data custodian, url = http://data.vancouver.ca/datacatalogue/publicPlaces.htm
schoolBoundaries <- fromJSON("elementary_school_boundaries.json")


# x and y coordinates are separated weirdly in the read data
# x are approx 49000, y are approx 54000. Since coord pairs, mid point is the split I want. Can be achieved by a triple index
  #schoolBoundaries$features$geometry$coordinates[[1]][, , 1]
  #schoolBoundaries$features$geometry$coordinates[[1]][, , 2]


# Step 3: Function definitions

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

# Step 4: API setup
############################################
##### Google routing distance API work #####
############################################

googleKey <- "AIzaSyDNSh3L8O1buhgauPwqqj6_Hb14HzgCXcg"

# Takes Lat, Long style coordinates
googleAPICaller <- function(key, origin, waypoints, dest){
  if(length(waypoints) > 0){
    originToSchool <- paste0("https://maps.googleapis.com/maps/api/directions/json?units=metric&origin=", origin, "&destination=", dest,
                             "&waypoints=optimize:true|", waypoints, "&mode=walking&key=", key)
  }
  else{
    originToSchool <- paste0("https://maps.googleapis.com/maps/api/directions/json?units=metric&origin=", origin, "&destination=", dest,
                             "&mode=walking&key=", key)
  }
  holder <- fromJSON(originToSchool)
  return(holder)
}

# Function that iterates over the n clusters we create
iterGoogleAPI <- function(googleKey, clusteredRoutes, schoolLocation){
  numCalls <- length(clusteredRoutes)
  routesAPI <- list()
  for(i in 1:numCalls){
    temp <- googleAPICaller(googleKey, clusteredRoutes[[i]][[1]], clusteredRoutes[[i]][[2]], schoolLocation)
    routesAPI[[i]] <- cbind(pathCollector(temp), cluster = i)
  }
  return(routesAPI)
}

# Function to extract path from googleAPI Call
pathCollector <- function(oneGoogleCall){
  steps <- oneGoogleCall$routes$legs[[1]]$steps
  holder <- steps[[1]]$start_location[1,]
  for(i in 1:length(steps)){
    holder <- rbind(holder, steps[[i]]$end_location)
  }
  return(holder)
}

# Function to extract time (seconds) and distance (metres) from the path
timeAndDistBySection <- function(oneGoogleCall){
  legs <- oneGoogleCall$routes$legs[[1]]
  dist <- legs$duration$value
  time <- legs$distance$value
  return(cbind(dist, time))
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
googleCall <- googleAPICaller(googleKey, charlesRoutes[[4]][[1]], charlesRoutes[[4]][[2]], charlesDickensLocation)
multiPaths <- iterGoogleAPI(googleKey, charlesRoutes, charlesDickensLocation)

# Graph with data points
schoolMap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14, maptype = "hybrid")
schoolMapWithPoints <- ggmap(schoolMap) + 
  geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
  geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
  geom_point(data = clustered, aes(x = x, y = y, col = clusters, shape = leader))

# Graph updated with paths
allPaths <- data.frame()
for(j in 1:length(multiPaths)){
  allPaths <- rbind(allPaths, multiPaths[[j]])
}
allPathsPlot <- schoolMapWithPoints + geom_path(data = allPaths, aes(x = lng, y = lat, size = 2, group = cluster, colour = as.factor(cluster)))
