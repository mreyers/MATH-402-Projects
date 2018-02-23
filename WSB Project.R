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
library(geosphere) # geodistances
library(clue) # Hungarian algorithm
library(dbscan)
library(ppclust) # fuzzy
library(Rfast) # rowMaxs
library(maptools) # simplymap data
library(raster) # plotting overlap
library(rgeos) # getting overlap

# Step 2: Grab a set of schools and their corresponding catchment regions  
# Testing the file Neggyn found on the data custodian, url = http://data.vancouver.ca/datacatalogue/publicPlaces.htm
schoolBoundaries <- fromJSON("elementary_school_boundaries.json")$features


# x and y coordinates are separated weirdly in the read data
# x are approx 49000, y are approx 54000. Since coord pairs, mid point is the split I want. Can be achieved by a triple index
  #schoolBoundaries$features$geometry$coordinates[[1]][, , 1]
  #schoolBoundaries$features$geometry$coordinates[[1]][, , 2]


# Step 3: Function definitions

######## Testing stuff for conversion of coordinate systems: Status = Working #########
# If the function breaks visit: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf

# Create a function that takes the jsonData and a given school and outputs its catchment map and bounding polygon. Will throw error for invalid school at the moment
schoolCatch <- function(jsonData, schoolName){
  index <- which(jsonData$properties$NAME == schoolName) # Select the school set of coords 
  Easting <- jsonData$geometry$coordinates[[index]][, , 1] # Separate to x and y
  Northing<- jsonData$geometry$coordinates[[index]][, , 2]
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
  clusters <- class::knn(train = centers[, 1:2], test = nodesAndLeaders[, 1:2], cl = cl, k = 1) %>% as.factor
  housesClustered <- nodesAndLeaders %>% add_column(clusters)
  return(housesClustered)
}

# Standard procedure to generate necessary info: Define school boundaries from file, sample points from polygon, assign leaders, group walkers, plot output


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
  routeMeasures <- list()
  for(k in 1:numCalls){
    temp <- googleAPICaller(googleKey, clusteredRoutes[[k]][[1]], clusteredRoutes[[k]][[2]], schoolLocation)
    routesAPI[[k]] <- cbind(pathCollector(temp), cluster = k)
    routeMeasures[[k]] <- timeAndDistBySection(temp)
  }
  return(list(routesAPI, routeMeasures))
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


################ Test Google Call for Charles Dickens and simulated data ##############

charlesDickensTest <- schoolCatch(schoolBoundaries, "Charles Dickens Elementary")
charlesSample <- polygons(30, charlesDickensTest)
lead <- leaders(charlesSample)
clustered <- groups(lead)
# Trying the routes out with clustered data, then with the testclust from below
charlesRoutes <- routeCreator(fuzzyResults)
charlesDickensLocation <- "49.254957,-123.083038"

# Test call for the 4th route: Key, origin, waypoints, destination
##googleCall <- googleAPICaller(googleKey, charlesRoutes[[4]][[1]], charlesRoutes[[4]][[2]], charlesDickensLocation)

# Iterator call for all routes
allRoutesToSchool <- iterGoogleAPI(googleKey, charlesRoutes, charlesDickensLocation)
routePaths <- allRoutesToSchool[[1]]
routeMeasures <- allRoutesToSchool[[2]]

# Graph with data points
schoolMap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14)
schoolMapWithPoints <- ggmap(schoolMap) + 
  geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
  geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
  geom_point(data = fuzzyResults, aes(x = x, y = y, col = as.factor(clusters), shape = leader))

# Graph updated with paths
allPaths <- data.frame()
for(j in 1:length(routePaths)){
  allPaths <- rbind(allPaths, routePaths[[j]])
}
allPathsPlot <- schoolMapWithPoints + geom_path(data = allPaths, aes(x = lng, y = lat, size = 2, group = cluster, colour = as.factor(cluster)))

# Collection of route properties (length, distance) stored in routeMeasures
# Convert these measures into walking times and distances of each student
studentTravels <- function(allRouteMeasures){
  numRoutes <- length(allRouteMeasures)
  studentMeasure <- data.frame(Distance = numeric(), Duration = numeric())
  for(i in 1:numRoutes){
    route <- allRouteMeasures[[i]]
    numStudents <- dim(allRouteMeasures[[i]])[1]
    for(j in 1:numStudents){
      studentDist <- sum(route[j:numStudents, 1])
      studentTime <- round(sum(route[j:numStudents, 2]) / 60, 2)
      student <- cbind(studentDist, studentTime)
      studentMeasure <- rbind(studentMeasure, student)
    }
  }
  return(studentMeasure)
}

testTravels4 <- studentTravels(routeMeasures)


############# FUNCTIONAL ##################
# Idea: kmeans clustering on non-leader nodes, join closest cluster to closest leader based on cluster centers. Each
recluster <- function(clusters){
  nonLeader <- clusters[clusters$leader != TRUE, ]
  leader <- clusters[clusters$leader == TRUE, ]
  meanResults <- kmeans(nonLeader[, 1:2], centers = dim(leader)[1], iter = 20)
  nonLeader$tempID <- meanResults$cluster
  distMat <- apply(meanResults$centers, MARGIN = 1, distGeo, p2 = leader[,1:2]) # columns are kmeans[i] versus each predefined leader
  optAssignments <- solve_LSAP(distMat)
  nonLeader$newClusters <- 0
  #print(optAssignments)
  for(i in 1:length(optAssignments)){
    nonLeader[nonLeader$tempID == i, "newClusters"] <- optAssignments[i]
  }
  #print(nonLeader)
  nonLeader <- nonLeader %>% mutate(clusters = newClusters) %>% select(-tempID, -newClusters)
  rownames(nonLeader) <- NULL
  rownames(leader) <- NULL
  reCluster <- rbind(leader, nonLeader)
  return(reCluster)
}

testClust <- recluster(clustered)

# Another function idea: Use hierarchical clustering based on geodesic distance matrix
hierClustering <- function(coordinates){
  leader <- coordinates[coordinates$leader == TRUE,]
  nonLeader <- coordinates[coordinates$leader == FALSE,]
  # convert data to a SpatialPointsDataFrame object
  xyCoords <- cbind(nonLeader$x, nonLeader$y)
  xy <- SpatialPointsDataFrame(
    matrix(xyCoords, ncol=2), data.frame(ID=seq(1:dim(xyCoords)[1])),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define the cluster threshold, in this case = to the number of leaders
  k <- dim(leader)[1]
  if( k <= 14){
    k <- k
  }
  else{
    k <- 14
  }
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  nonLeader$clusters <- cutree(hc, k = k)
  rownames(nonLeader) <- NULL
  leaderClusters <- class::knn(train = nonLeader[, 1:2], test = leader[, 1:2], cl = nonLeader$clusters, k = 4)
  leader$clusters <- leaderClusters
  rownames(leader) <- NULL
  clusters <- rbind(leader, nonLeader)
  return(clusters)
}

testSet2 <- hierClustering(clustered)


# Fuzzy clustering works for path generation though it seems a leader is occasionally misclassified despite its route going fine
fuzzyClusters <- function(coordinates){
  
  # Do the fuzzy matching and select the probability matrix from the output (column u)
  fuzzyMatch <- fcm(coordinates[, 1:2], centers = coordinates[coordinates$leader == TRUE, 1:2]) # Centers are the route leaders
  fuzzyMatch <- fuzzyMatch$u
  
  # Choose cluster ID based on maximum probability
  maxRows <- rowMaxs(fuzzyMatch)
  
  # Add the cluster id's to the data set
  coordinates$clusters <- maxRows
  return(coordinates)
}

fuzzyResults <- fuzzyClusters(clustered) #537.99

# Based on the first set of data used, fuzzy clustering looks to be the best fit for this work though we can do more tests to verify

####################################
# Testing the above functions over multiple catchments and situations #
####################################

# Get school coordinates 
View(schoolBoundaries)

# test.csv has geocoordinates for the schools, load them in and filter the clean ones that were well located
schoolLocations <- read.csv("test.csv")
schoolBoundaries$properties$lat <- schoolLocations$lat
schoolBoundaries$properties$long<- schoolLocations$long

# Schools that can be easily iterated through
easySchoolsCoords <- schoolBoundaries$properties %>% 
                                      filter(lat >49 &
                                             lat <50 &
                                             long > c(-124) &
                                             long < c(-122))

# We have two Lord Strathcona Elemtentaries, need to remove both because of run errors probably due to how data recorded
easySchoolsPolygons <- schoolBoundaries[schoolBoundaries$properties$NAME %in% easySchoolsCoords$NAME & schoolBoundaries$properties$NAME != "Lord Strathcona Community Elementary",]
# Error occurring at Shaugnessy as well, look into the cause

easySchoolsPolygons$stringCoords <- paste0(easySchoolsPolygons$properties$lat, ",", easySchoolsPolygons$properties$long)
rownames(easySchoolsPolygons)
# Now to build the structure of iterating based on the usable schools and the structure laid out above

allPathsAllSchools <- rep(list(NA), length(easySchoolsPolygons$properties$NAME))
schoolPolygonStore <- list()
for(i in 1:length(easySchoolsPolygons$properties$NAME)){
  # Loop over each name in the easy schools data set
  
  # Set the bounding polygon and sample from it
  schoolTest <- schoolCatch(easySchoolsPolygons, easySchoolsPolygons$properties$NAME[i])
  schoolSample <- polygons(30, schoolTest)
  
  # Create a polygon object for the school's boundary to use later
  poly1 <- Polygon(schoolTest)
  schoolPolygonStore[[i]] <- Polygons(list(poly1), i)
  
  # Determine the leaders and build the clusters, though clustering can be done differently
  schoolLead <- leaders(schoolSample)
  schoolClustered <- groups(schoolLead) # Can recluster schoolClustered according to the functions designed
  
  # Build routes and identify the location of the school
  schoolRoutes <- routeCreator(schoolClustered)
  schoolLocation <- easySchoolsPolygons$stringCoords[i]

  # Iterator call for all routes to all schools. Permanent results will be stored later so this is not interferred with
  allRoutesToAllSchools <- iterGoogleAPI(googleKey, schoolRoutes, schoolLocation)
  routePathsAll <- allRoutesToAllSchools[[1]]
  routeMeasuresAll <- allRoutesToAllSchools[[2]]

  # Graph with data points
  schoolMap <- get_map(location = c(lon = easySchoolsPolygons$properties$long[i] ,lat = easySchoolsPolygons$properties$lat[i]), zoom = 14)
  
  assign(paste0(easySchoolsPolygons$properties$NAME[i]),  (ggmap(schoolMap) + 
    geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "black", alpha = 0.3, shape = 13)) + theme(legend.position = "none") + 
    geom_polygon(data = schoolTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
    geom_point(data = schoolClustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))))
  
  # Do it a second time so it is easy to call the map for route setting
  schoolMapWithPoints <- ggmap(schoolMap) + 
    geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
    geom_polygon(data = schoolTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
    geom_point(data = schoolClustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))

# Graph updated with paths

for(j in 1:length(routePathsAll)){
  allPathsAllSchools[[i]] <- rbind(allPathsAllSchools[[i]], routePathsAll[[j]])
}
allPathsAllSchools[[i]] <- allPathsAllSchools[[i]] %>% filter(!is.na(lat))

assign(paste0(easySchoolsPolygons$properties$NAME[i], " Paths"), (schoolMapWithPoints + geom_path(data = allPathsAllSchools[[i]], aes(x = lng, y = lat, size = 2, group = cluster, colour = as.factor(cluster)))))

}



############ NEW SAMPLING STUFF #################
# Data is saved in the spatialfiles folder in downloads
# Figure out how to grab and plot the coordinates from the object


area <- readShapePoly("SimplyAnalytics_Shapefiles_2018-02-20_21_22_35_0023a1e3447fdb31836536cc903f1310.shp", delete_null_obj = TRUE)
area@plotOrder

View(area)
str(area@polygons@Polygons)

####### Getting the overlap ############
# School polygon boundaries converted into Spatial Polygons
together <- SpatialPolygons(schoolPolygonStore, 1:length(schoolPolygonStore))

# Turn area dataframe into a list
togetherAreaList[1] <- area[1,]@polygons
for(l in 2:length(area)){
  togetherAreaList[l] <- area[l,]@polygons
}
togetherArea <- SpatialPolygons(togetherAreaList, 1:length(area))

# Find the intersection of the census data and school boundaries
intersections <- intersect(togetherArea, together)

# Adjust projections 
projection(togetherArea) <- projection(together)

# Plot the two maps overlaid by colour of overlap
plot(togetherArea); plot(intersections, add=T, col=alpha('red', 0.2)); plot(together, axes=T, add = T, border = 1:length(together), lwd = 2); 


# Helpers down below, things to help understand the above flow if I get lost 

# Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
# Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
# Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
# Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
# 
# Srs1 = Polygons(list(Sr1), "s1")
# Srs2 = Polygons(list(Sr2), "s2")
# Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
# SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
# plot(SpP, col = 1:3, pbg="white")
