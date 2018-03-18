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
library(data.table) # turning lists into write.csv data frames

# Step 1: Set seed for reproducibility when running from start
set.seed(1)

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
  
  # If we didnt get any leaders, resample
  if( length((uniPoints > 0.65)) == 0){
    uniPoints <- runif(nPoints)
    leader <- (uniPoints > 0.65)
  }
  # If we got too many leaders, sample from them
  if( length((uniPoints > 0.65)) >= 15){
    leader <- (uniPoints > 0.75)
  }
  
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

googleKey <- "AIzaSyCO6s6_vat14sb5HDNvayx7JN9h8XXrxzc"

# Takes Lat, Long style coordinates
googleAPICaller <- function(key, origin, waypoints, dest){
  #print(length(waypoints))
  if(length(waypoints) > 0){
    originToSchool <- paste0("https://maps.googleapis.com/maps/api/directions/json?units=metric&origin=", origin, "&destination=", dest,
                             "&waypoints=optimize:true|", waypoints, "&mode=walking&key=", key)
  }
  else{
    originToSchool <- paste0("https://maps.googleapis.com/maps/api/directions/json?units=metric&origin=", origin, "&destination=", dest,
                             "&mode=walking&key=", key)
    print(originToSchool)
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
  clusterDF <- clusterDF %>% arrange(clusters)
  leaders <- clusterDF %>% filter(leader == TRUE)
  nodes <- clusterDF %>% filter(leader != TRUE)
  #print(leaders)
  #print(nodes)
  #textData <- nodes %>% mutate(combo = paste0(.$y, ",", .$x))
  textData <- nodes %>% mutate(combo = paste0(.$y, ",", .$x))
  #routeList <- split(textData, c(textData$clusters, textData$leader))
  routeList <- list()
  for( i in 1:dim(leaders)[1]){#for each leader
    routeList[[i]] <- textData[textData$clusters == i,]
   # print(dim(routeList[[i]])[1])
    routeList[[i]]
  }
  
  routes <- list()
  result <- list()
  #print(length(routeList))
  for(i in 1:length(routeList)){
   # print(i)
    temp <- with(leaders[i,], paste0(y, ",", x))
    if(dim(routeList[[i]])[1] > 0){
      #print("if")
      #print(list(temp, with(routeList[[i]], paste0(combo)) %>% str_c(collapse = "|")))
      result[[i]] <- list(temp, with(routeList[[i]], paste0(combo)) %>% str_c(collapse = "|"))
    }
    else{
      #print("else")
      result[[i]] <- list(temp, "")
    }
    #print(result[[i]])
  }
  return(result)
}
#routeCreator(allVRPResults[[1]])


################ Test Google Call for Charles Dickens and simulated data ##############

#  charlesDickensTest <- schoolCatch(schoolBoundaries, "Charles Dickens Elementary")
#  charlesSample <- polygons(30, charlesDickensTest)
# 
#  # Trying some distance matrix stuff to see if we can make this happen
#  
#  distanceAPIKey <- "AIzaSyDpnZH86z5GgKAZ3o5l287ghqEoFtBH8QA" # New key
#  charlesDickensLocation <- "49.254957,-123.083038"
#  
#  sampleWithSchool <- charlesSample
#  lead <- leaders(sampleWithSchool)
#  clustered <- groups(lead)
#  sampleWithSchool[31, ] <- c(-123.083038, 49.254957)
#  library(netgen) # Vehicle routing approach
#  # Figure out how to generate the clusters according to the VRP
#  # Use clusters with TSP or other optim approach
#   # Might require building an arc from school to cluster nodes with 0 weight, keep reading
#  
#  # Going to do this part in Excel, will make the following changes
#   # Depot to leader dist = 0
#   # Leader to other leader dist = 1000 (or could modify the cost)
#  leaderDistToChange <- which(clustered$leader == TRUE)
#  fullDist <- dist(sampleWithSchool) %>% as.matrix() #%>% ETSP()
#  # Specifically alter the distance from the leaders to the school to be 0
#  fullDist[31, leaderDistToChange] <- 0
#  # Apply the VRP to this altered distance matrix
#  
#  
#  # Trying the routes out with clustered data, then with the testclust from below
#  charlesRoutes <- routeCreator(clustered)
#  charlesDickensLocation <- "49.254957,-123.083038"
# 
# # Test call for the 4th route: Key, origin, waypoints, destination
# # googleCall <- googleAPICaller(googleKey, charlesRoutes[[4]][[1]], charlesRoutes[[4]][[2]], charlesDickensLocation)
# 
# # # Iterator call for all routes
#  allRoutesToSchool <- iterGoogleAPI(googleKey, charlesRoutes, charlesDickensLocation)
#  routePaths <- allRoutesToSchool[[1]]
#  routeMeasures <- allRoutesToSchool[[2]]
# # 
# # # Graph with data points
#  schoolMap <- get_map(location = c(lon = -123.083038 ,lat = 49.254957), zoom = 14)
#  schoolMapWithPoints <- ggmap(schoolMap) + 
#    geom_point(aes(x = -123.083038, y = 49.254957, size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
#    geom_polygon(data = charlesDickensTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
#    geom_point(data = clustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))
# # 
# # # Graph updated with paths
#  allPaths <- data.frame()
#  for(j in 1:length(routePaths)){
#    allPaths <- rbind(allPaths, routePaths[[j]])
#  }
#  allPathsPlot <- schoolMapWithPoints + geom_path(data = allPaths, aes(x = lng, y = lat, size = 2, group = cluster, colour = as.factor(cluster)))

# Collection of route properties (length, distance) stored in routeMeasures
# Convert these measures into walking times and distances of each student
studentTravels <- function(allRouteMeasures){
  numRoutes <- length(allRouteMeasures)
  studentMeasure <- data.frame(Distance = numeric(), Duration = numeric())
  leader <- vector()
  clusters <- vector()
  k <- 1
  for(i in 1:numRoutes){
    route <- allRouteMeasures[[i]]

    numStudents <- dim(allRouteMeasures[[i]])[1]

    for(j in 1:numStudents){
      if( j == 1){
        leader[k] = TRUE
      }
      else{
        leader[k] = FALSE
      }
      clusters[k] <- i
      studentDist <- sum(route[j:numStudents, 1])
      studentTime <- round(sum(route[j:numStudents, 2]) / 60, 2)
      student <- cbind(studentDist, studentTime)
      studentMeasure <- rbind(studentMeasure, student)
      k <- k + 1
    }
  }
  studentMeasure <- cbind(studentMeasure, leader, clusters)
  return(studentMeasure)
}



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
  nonLeader <- nonLeader %>% mutate(clusters = newClusters) %>% dplyr::select(-tempID, -newClusters)
  rownames(nonLeader) <- NULL
  rownames(leader) <- NULL
  reCluster <- rbind(leader, nonLeader)
  return(reCluster)
}

# testClust <- recluster(clustered)

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

# testSet2 <- hierClustering(clustered)


# Fuzzy clustering works for path generation though it seems a leader is occasionally misclassified despite its route going fine
fuzzyClusters <- function(coordinates){
  
  # Do the fuzzy matching and select the probability matrix from the output (column u)
  fuzzyMatch <- fcm(coordinates[coordinates$leader == FALSE, 1:2], centers = coordinates[coordinates$leader == TRUE, 1:2]) # Centers are the route leaders
  fuzzyMatch <- fuzzyMatch$u
  
  # Choose cluster ID based on maximum probability, not changing leader
  maxRows <- rowMaxs(fuzzyMatch)
  
  # Add the cluster id's to the data set
  coordinates$clusters[coordinates$leader == FALSE] <- maxRows
  return(coordinates)
}

# fuzzyResults <- fuzzyClusters(clustered) #537.99

# Based on the first set of data used, fuzzy clustering looks to be the best fit for this work though we can do more tests to verify

####################################
# Testing the above functions over multiple catchments and situations #
####################################

# Get school coordinates 
# View(schoolBoundaries)

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


# Old but functional, commented out to save runtime

# allPathsAllSchools <- rep(list(NA), length(easySchoolsPolygons$properties$NAME))
# schoolPolygonStore <- list()
# for(i in 1:length(easySchoolsPolygons$properties$NAME)){
#   # Loop over each name in the easy schools data set
#   
#   # Set the bounding polygon and sample from it
#   schoolTest <- schoolCatch(easySchoolsPolygons, easySchoolsPolygons$properties$NAME[i])
#   schoolSample <- polygons(30, schoolTest)
#   
#   # Create a polygon object for the school's boundary to use later
#   poly1 <- Polygon(schoolTest)
#   schoolPolygonStore[[i]] <- Polygons(list(poly1), i)
#   
#   # Determine the leaders and build the clusters, though clustering can be done differently
#   schoolLead <- leaders(schoolSample)
#   schoolClustered <- groups(schoolLead) # Can recluster schoolClustered according to the functions designed
#   
#   # Build routes and identify the location of the school
#   schoolRoutes <- routeCreator(schoolClustered)
#   schoolLocation <- easySchoolsPolygons$stringCoords[i]
# 
#   # Iterator call for all routes to all schools. Permanent results will be stored later so this is not interferred with
#   allRoutesToAllSchools <- iterGoogleAPI(googleKey, schoolRoutes, schoolLocation)
#   routePathsAll <- allRoutesToAllSchools[[1]]
#   routeMeasuresAll <- allRoutesToAllSchools[[2]]
# 
#   # Graph with data points
#   schoolMap <- get_map(location = c(lon = easySchoolsPolygons$properties$long[i] ,lat = easySchoolsPolygons$properties$lat[i]), zoom = 14)
#   
#   assign(paste0(easySchoolsPolygons$properties$NAME[i]),  (ggmap(schoolMap) + 
#     geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "black", alpha = 0.3, shape = 13)) + theme(legend.position = "none") + 
#     geom_polygon(data = schoolTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
#     geom_point(data = schoolClustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))))
#   
#   # Do it a second time so it is easy to call the map for route setting
#   schoolMapWithPoints <- ggmap(schoolMap) + 
#     geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
#     geom_polygon(data = schoolTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
#     geom_point(data = schoolClustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))
# 
# # Graph updated with paths
# 
# for(j in 1:length(routePathsAll)){
#   allPathsAllSchools[[i]] <- rbind(allPathsAllSchools[[i]], routePathsAll[[j]])
# }
# allPathsAllSchools[[i]] <- allPathsAllSchools[[i]] %>% filter(!is.na(lat))
# 
# assign(paste0(easySchoolsPolygons$properties$NAME[i], " Paths"), (schoolMapWithPoints + geom_path(data = allPathsAllSchools[[i]], aes(x = lng, y = lat, size = 2, group = cluster, colour = as.factor(cluster)))))
# 
# }



############ NEW SAMPLING STUFF #################
# Data is saved in the spatialfiles folder in downloads
# Figure out how to grab and plot the coordinates from the object
# This stuff is to be done globally as it relates each data set to the other.
  # Responsible for overlap of schoolcatchments and census data

schoolPolygonStore <- list()
for(i in 1:length(easySchoolsPolygons$properties$NAME)){
  # Loop over each name in the easy schools data set
  
  # Set the bounding polygon and sample from it
  schoolTest <- schoolCatch(easySchoolsPolygons, easySchoolsPolygons$properties$NAME[i])
  schoolSample <- polygons(30, schoolTest)
  
  # Create a polygon object for the school's boundary to use later
  poly1 <- Polygon(schoolTest)
  schoolPolygonStore[[i]] <- Polygons(list(poly1), i)
}

area <- readShapePoly("SimplyAnalytics_Shapefiles_2018-02-20_21_22_35_0023a1e3447fdb31836536cc903f1310.shp", delete_null_obj = TRUE)
# area@plotOrder

# View(area)

####### Getting the overlap ############
# School polygon boundaries converted into Spatial Polygons
together <- SpatialPolygons(schoolPolygonStore, 1:length(schoolPolygonStore))

# Turn area dataframe into a list
togetherAreaList <- list()
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
plot(togetherArea); plot(intersections, add=T, col=alpha('red', 0.2)); plot(together, axes=T, add = T, border = 1:length(together), lwd = 2)


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

########################### SAMPLING STUFF TO BE MADE INTO A FUNCTION ##########################
# Try to get spatial polygon sampling based on census results of each area
  # Need to first get all the true instances from intersections 
  easyIntersections <- gIntersects( together, togetherArea, byid = T) # Global

# The function to do all the sampling
overlapSampler <- function(schoolNum){
  # We now have 992 rows and 57 columns for schools. Consider the census areas for school 1
  interArea <- easyIntersections[which(easyIntersections[,schoolNum] == TRUE), schoolNum]
  
  # Create a new plot to verify this
  # schoolpolygonStore should be indexed the same way easyIntersections is
  togetherTest <- SpatialPolygons(list(schoolPolygonStore[[schoolNum]]), 1:1)
  intersectionsTest <- intersect(togetherArea, togetherTest)
  
  # Adjust projections 
  projection(togetherArea) <- projection(togetherTest)
  
  # Now we need to get the data from each of these polygons w.r.t. the population contained
  area$rownames <- rownames(easyIntersections)
  sizeOfRegion <- area[area$rownames %in% names(interArea),]$VALUE0
  
  # Now using the size of region we can define a pseudo probability selector
  sizeOfRegion <- sizeOfRegion / sum(sizeOfRegion)
  
  # We can now do weighted sampling, using the sizeOfRegion as a probability generator
  polygonsToUse <- sample(names(interArea), size = 30, prob = sizeOfRegion, replace = TRUE)
  
  # Create the adjusted sampling zone
  adjustedSamplingZone <- list()
  for(i in 1:length(names(interArea))){
    adjustedSamplingZone[[i]] <- gIntersection(togetherTest, area[area$rownames %in% names(interArea)[i],])
  }
  
  # Sample the points according to spsample
  numberPerRegion <- data.frame(polygonsToUse, stringsAsFactors = FALSE) %>% group_by(polygonsToUse) %>% summarize(n = n())
  houses <- data.frame()
  
  j <- 1 # Manual iteration over the other dataframe as it is indexed differently
  for(i in 1:length(names(interArea))){
    if(names(interArea)[i] %in% numberPerRegion$polygonsToUse){
      Ps <- SpatialPolygons(adjustedSamplingZone[[i]]@polygons, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      samp <- spsample(Ps, n = as.numeric(numberPerRegion[j, "n"]), type = "random", iter = 10)@coords %>% as.data.frame()
      names(samp) <- c("x", "y")
      houses <- rbind(houses, samp)
      j <- j + 1
    }
  }
  return(list(togetherTest, houses))
}

proportionalSampleRegion <- list()
proportionalSampleHouses <- list()

for( i in 1:length(together)){
  overlap <- overlapSampler(i)
  proportionalSampleRegion[[i]] <- overlap[1]
  proportionalSampleHouses[[i]] <- overlap[2]
}


plot(togetherArea); 
#plot(intersectionsTest, add=T, col=alpha('red', 0.2)); 
plot(proportionalSampleRegion[[42]][[1]], axes=T, add = T, border = 1:length(together), lwd = 2); 
points(proportionalSampleHouses[[42]][[1]], col = "green", lwd = 2)

# proportionalSampleRegion and proportionalSampleHouses now contain the desired regions and data points
# Now the points need to be clustered and routes created
allRoutesToAllSchools <- list(list(NA), length(easySchoolsPolygons$properties$NAME))
routePathsAll <- list(list(NA), length(easySchoolsPolygons$properties$NAME))
routeMeasuresAll <- list(list(NA), length(easySchoolsPolygons$properties$NAME))
scoringResults <- data.frame(knn = NA, kmeans = NA, hier = NA, fuzzy = NA)
allTheClusters <- list()
for( i in 1:length(easySchoolsPolygons$properties$NAME)){
  lead <- leaders(proportionalSampleHouses[[i]][[1]])
  
  clustered <- groups(lead)
  allTheClusters[[i]] <- clustered
  # kmeansClust <- recluster(clustered)
  # hierClust <- hierClustering(clustered)
  # fuzzyClust <- fuzzyClusters(clustered)
  # 
  # routes <- list()
  # routes[[1]] <- routeCreator(clustered)
  # routes[[2]] <- routeCreator(kmeansClust)
  # routes[[3]] <- routeCreator(hierClust)
  # routes[[4]] <- routeCreator(fuzzyClust)
  # 
  # schoolLocation <- easySchoolsPolygons$stringCoords[i]
  # allClustersToASchool <- list()
  # pathHolder <- list(NA, 4)
  # measureHolder <- list(NA, 4)
  # for(j in 1:4){
  #   print(paste0(i, " ", j))
  #   allClustersToASchool <- iterGoogleAPI(googleKey, routes[[j]], schoolLocation)
  #   pathHolder[[j]] <- allClustersToASchool[[1]]
  #   measureHolder[[j]] <- allClustersToASchool[[2]]
  # }
  # 
  # routePathsAll[[i]] <- pathHolder
  # routeMeasuresAll[[i]] <- measureHolder
}





  # Probably do scoring function here and proceed with best score
  # Pseudo code for later
  #scoringResults$column <- clusteredTypeResults  
  

  
  
  # schoolMap <- get_map(location = c(lon = easySchoolsPolygons$properties$long[i] ,lat = easySchoolsPolygons$properties$lat[i]), zoom = 14)
  # sampleRegionSimplified <- proportionalSampleRegion[[i]][[1]]@polygons[[1]]@Polygons[[1]]@coords %>% as.data.frame()
  # 
  #   assign(paste0(easySchoolsPolygons$properties$NAME[i]),  (ggmap(schoolMap) +
  #     geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "black", alpha = 0.3, shape = 13)) + theme(legend.position = "none") +
  #     geom_polygon(data = sampleRegionSimplified, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") +
  #     geom_point(data = clustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))))
  # 
  #   # Do it a second time so it is easy to call the map for route setting
  #   schoolMapWithPoints <- ggmap(schoolMap) +
  #     geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") +
  #     geom_polygon(data = sampleRegionSimplified, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") +
  #     geom_point(data = clustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))


# routeMeasuresAll[[SampleRegion]][[ClusteringMethod]][[GroupMetrics]] 
# routeMeasuresAll[[1]][[2]][[3]]

# Need to find more convenient way to stack these dataset
testRoutes <- routeMeasuresAll
testOutput <- lapply(testRoutes[[1]], studentTravels)
crazyTest <- lapply(testRoutes, function(x) lapply(x, studentTravels))
crazyRbind <- lapply(crazyTest, rbindlist) # Keep working here, try to pipe it better to a data frame for everything and a column for which observation we are on
saveDF <- data.frame()
for(i in 1:length(crazyRbind)){
  clusteringMethodVec <- rep(c("kNN", "kMeans", "Hier", "Fuzzy"), each = 30)
  routeCovered <- rep(i, each = 120)
  crazyRbind[[i]]$ClustAlg <- clusteringMethodVec
  crazyRbind[[i]]$Catchment <- routeCovered
  saveDF <- rbind(saveDF, crazyRbind[[i]])
}



# Read in the data from the VRP on the 10 regions to verify
region55 <- read.csv("region55Data.csv")
region35 <- read.csv("region35Data.csv")
region9  <- read.csv("region9Data.csv")
region29 <- read.csv("region29Data.csv")
region57 <- read.csv("region57Data.csv")
region34 <- read.csv("school34.csv")
region8  <- read.csv("school8.csv")
region44 <- read.csv("school44.csv")
region45 <- read.csv("school45.csv")
region22 <- read.csv("school22.csv")


region55$X <- as.character(region55$X)
clusterList <- list()
for(i in 6:dim(region55)[2]){
  # V1 is in spot 5 so cluster 1 is i - 4
  nodes <- levels(region55[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:length(nodes)]
  clusterList[[i - 5]] <- nodes
  region55$clusters[region55$X %in% clusterList[[i-5]]] <- i-5
  
}
region55 <- region55[, 2:5]

region35$X <- as.character(region35$X)
clusterList <- list()
for(i in 6:dim(region35)[2]){
  # V1 is in spot 5 so cluster 1 is i - 4
  nodes <- levels(region35[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:length(nodes)]
  clusterList[[i - 5]] <- nodes
  region35$clusters[region35$X %in% clusterList[[i-5]]] <- i-5
  
}
region35 <- region35[, 2:5]

region9$X <- as.character(region9$X)
clusterList <- list()
for(i in 6:dim(region9)[2]){
  # V1 is in spot 5 so cluster 1 is i - 4
  nodes <- levels(region9[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:length(nodes)]
  clusterList[[i - 5]] <- nodes
  region9$clusters[region9$X %in% clusterList[[i-5]]] <- i-5
  
}
region9 <- region9[, 2:5]

region29$X <- as.character(region29$X)
clusterList <- list()
for(i in 6:dim(region29)[2]){
  # V1 is in spot 5 so cluster 1 is i - 4
  nodes <- levels(region29[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:length(nodes)]
  clusterList[[i - 5]] <- nodes
  region29$clusters[region29$X %in% clusterList[[i-5]]] <- i-5
  
}
region29 <- region29[, 2:5]

region57$X <- as.character(region57$X)
clusterList <- list()
for(i in 6:dim(region57)[2]){
  # V1 is in spot 5 so cluster 1 is i - 4
  nodes <- levels(region57[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:length(nodes)]
  clusterList[[i - 5]] <- nodes
  region57$clusters[region57$X %in% clusterList[[i-5]]] <- i-5
  
}
region57 <- region57[, 2:5]

# CHECK THIS REGION AGAIN
region34$X <- as.character(region34$X)
clusterList <- list()
clusterDF <- data.frame(ID = as.character(1:30), clusters = rep(NA, 30))
for(i in 5:dim(region34)[2]){
  nodes <- levels(region34[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:(length(nodes) - 1)]
  clusterList[[i-4]] <- nodes
  clusterDF$clusters[clusterDF$ID %in% clusterList[[i-4]]] <- i-4
}
region34$clusters <- clusterDF$clusters
region34 <- region34 %>% dplyr::select(x, y, leader, clusters)

region8$X <- as.character(region8$X)
clusterList <- list()
clusterDF <- data.frame(ID = as.character(1:30), clusters = rep(NA, 30))
for(i in 5:dim(region8)[2]){
  nodes <- levels(region8[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:(length(nodes) - 1)]
  clusterList[[i-4]] <- nodes
  clusterDF$clusters[clusterDF$ID %in% clusterList[[i-4]]] <- i-4
}
region8$clusters <- clusterDF$clusters
region8 <- region8 %>% dplyr::select(x, y, leader, clusters)


region44$X <- as.character(region44$X)
clusterList <- list()
clusterDF <- data.frame(ID = as.character(1:30), clusters = rep(NA, 30))
for(i in 5:dim(region44)[2]){
  nodes <- levels(region44[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:(length(nodes) - 1)]
  clusterList[[i-4]] <- nodes
  clusterDF$clusters[clusterDF$ID %in% clusterList[[i-4]]] <- i-4
}
region44$clusters <- clusterDF$clusters
region44 <- region44 %>% dplyr::select(x, y, leader, clusters)


region45$X <- as.character(region45$X)
clusterList <- list()
clusterDF <- data.frame(ID = as.character(1:30), clusters = rep(NA, 30))
for(i in 5:dim(region45)[2]){
  nodes <- levels(region45[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:(length(nodes) - 1)]
  clusterList[[i-4]] <- nodes
  clusterDF$clusters[clusterDF$ID %in% clusterList[[i-4]]] <- i-4
}
region45$clusters <- clusterDF$clusters
region45 <- region45 %>% dplyr::select(x, y, leader, clusters)


region22$X <- as.character(region22$X)
clusterList <- list()
clusterDF <- data.frame(ID = as.character(1:30), clusters = rep(NA, 30))
for(i in 5:dim(region22)[2]){
  nodes <- levels(region22[, i]) %>% str_replace("Customer ", "")
  nodes <- nodes[2:(length(nodes) - 1)]
  clusterList[[i-4]] <- nodes
  clusterDF$clusters[clusterDF$ID %in% clusterList[[i-4]]] <- i-4
}
region22$clusters <- clusterDF$clusters
region22 <- region22 %>% dplyr::select(x, y, leader, clusters)


# All of the sampled regions now have their data in the clustered format meaning we can evaluate them as above

allVRPResults <- list(region8, region9, region22, region29, region34, region35, region44, region45, region55, region57)
schoolNames <- easySchoolsPolygons$properties$NAME[c(8, 9, 22, 29, 34, 35, 44, 45, 55, 57)]
routesVRP <- list()
measuresVRP <- list()
for(i in 1:10){
  routes <- routeCreator(allVRPResults[[i]])
  schoolLocation <- easySchoolsPolygons$stringCoords[easySchoolsPolygons$properties$NAME %in% schoolNames[i]]
  allClustersToASchool <- iterGoogleAPI(googleKey, routes, schoolLocation)
  routesVRP[[i]] <- allClustersToASchool[[1]]
  measuresVRP[[i]] <- allClustersToASchool[[2]]
}

# TO DO: Convert this into desired format (crazyRbind) and pass data to Neggyn
# Verify the results

VRPTest <- measuresVRP
VRPTestOutput <- lapply(VRPTest, studentTravels)

crazyRbind <- rbindlist(VRPTestOutput) # Keep working here, try to pipe it better to a data frame for everything and a column for which observation we are on
schoolNameIndex <- c()
for( i in 1:300){
  schoolNameIndex[i] <- ceiling(i/30)
}
saveDF <- crazyRbind %>% cbind(schoolNameIndex)
write.csv(saveDF, "VRPScores.csv")







#   # Build routes and identify the location of the school
#   schoolRoutes <- routeCreator(schoolClustered)
#   schoolLocation <- easySchoolsPolygons$stringCoords[i]
# 
#   # Iterator call for all routes to all schools. Permanent results will be stored later so this is not interferred with
#   
#   routePathsAll <- allRoutesToAllSchools[[1]]
#   routeMeasuresAll <- allRoutesToAllSchools[[2]]
# 
#   # Graph with data points
#   schoolMap <- get_map(location = c(lon = easySchoolsPolygons$properties$long[i] ,lat = easySchoolsPolygons$properties$lat[i]), zoom = 14)
#   
#   assign(paste0(easySchoolsPolygons$properties$NAME[i]),  (ggmap(schoolMap) + 
#     geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "black", alpha = 0.3, shape = 13)) + theme(legend.position = "none") + 
#     geom_polygon(data = schoolTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
#     geom_point(data = schoolClustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))))
#   
#   # Do it a second time so it is easy to call the map for route setting
#   schoolMapWithPoints <- ggmap(schoolMap) + 
#     geom_point(aes(x = easySchoolsPolygons$properties$long[i], y = easySchoolsPolygons$properties$lat[i], size = 3, col = "red", alpha = 0.3)) + theme(legend.position = "none") + 
#     geom_polygon(data = schoolTest, aes(x = Longitude, y = Latitude), alpha = 0.3, colour = "red", fill = "red") + 
#     geom_point(data = schoolClustered, aes(x = x, y = y, col = as.factor(clusters), shape = leader))
# 
# # Graph updated with paths
# 
# for(j in 1:length(routePathsAll)){
#   allPathsAllSchools[[i]] <- rbind(allPathsAllSchools[[i]], routePathsAll[[j]])
# }
# allPathsAllSchools[[i]] <- allPathsAllSchools[[i]] %>% filter(!is.na(lat))
# 
# assign(paste0(easySchoolsPolygons$properties$NAME[i], " Paths"), (schoolMapWithPoints + geom_path(data = allPathsAllSchools[[i]], aes(x = lng, y = lat, size = 2, group = cluster, colour = as.factor(cluster)))))
# 
# }


















# 
#   # The rows of easyIntersections are the schools (57), the columns are the census areas (992)
#   # So for a given school we can identify the census areas it crosses if we take the transpose
#   # easyIntersections <- t(easyIntersections)
#   
#   # We now have 992 rows and 57 columns for schools. Consider the census areas for school 1
#   interArea <- easyIntersections[which(easyIntersections[,2] == TRUE), 2]
#   
#   # The results for school 1 show that the overlap exists with these regions: "29"  "427" "428" "786" "788" "791" "792" "793" "794" "795" "796" "797" "798" "801" "802" "836" "837"
#   # Create a new plot to verify this
#   togetherTest <- SpatialPolygons(list(schoolPolygonStore[[2]]), 1:1)
#   intersectionsTest <- intersect(togetherArea, togetherTest)
#   
#   # Adjust projections 
#   projection(togetherArea) <- projection(togetherTest)
#   
#   # Plot the two maps overlaid by colour of overlap
#   plot(togetherArea); plot(intersectionsTest, add=T, col=alpha('red', 0.2)); plot(togetherTest, axes=T, add = T, border = 1:length(together), lwd = 2)
#   
#   # Good, plot shows as desired
#   # Now we need to get the data from each of these polygons w.r.t. the population contained
#   area$rownames <- rownames(easyIntersections)
#   sizeOfRegion <- area[area$rownames %in% names(interArea),]$VALUE0
#   
#   # Now using the size of region we can define a pseudo probability selector
#   sizeOfRegion <- sizeOfRegion / sum(sizeOfRegion)
#   
#   # We can now do weighted sampling, using the sizeOfRegion as a probability generator
#   polygonsToUse <- sample(names(interArea), size = 30, prob = sizeOfRegion, replace = TRUE)
#   
#   # Adjust the sampling area of each zone to its intersection with the overall catchment
#   plot(togetherArea) 
#   adjustedSamplingZone <- list()
#   for(i in 1:length(names(interArea))){
#     adjustedSamplingZone[[i]] <- gIntersection(togetherTest, area[area$rownames %in% names(interArea)[i],])
#     plot(adjustedSamplingZone[[i]], add=T, col = "blue")
#   }
#   plot(togetherTest, axes=T, add = T, border = 1:length(together), lwd = 2); 
#   # Plot created here shows that the zones in adjustedSamplingZone can be correctly sampled from
#   
#   # Sample the points according to spsample
#   numberPerRegion <- data.frame(polygonsToUse, stringsAsFactors = FALSE) %>% group_by(polygonsToUse) %>% summarize(n = n())
#   houses <- data.frame()
#   j <- 1 # Manual iteration over the other dataframe as it is indexed differently
#   for(i in 1:length(names(interArea))){
#     if(names(interArea)[i] %in% numberPerRegion$polygonsToUse){
#       Ps <- SpatialPolygons(adjustedSamplingZone[[i]]@polygons, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#       samp <- spsample(Ps, n = as.numeric(numberPerRegion[j, "n"]), type = "random", iter = 10)@coords %>% as.data.frame()
#       names(samp) <- c("x", "y")
#       houses <- rbind(houses, samp)
#       j <- j + 1
#     }
#   }
#   
#   
#   
#   # Check if they plot correctly
#   plot(togetherArea); 
#   #plot(intersectionsTest, add=T, col=alpha('red', 0.2)); 
#   plot(togetherTest, axes=T, add = T, border = 1:length(together), lwd = 2); 
#   points(houses, col = "green", lwd = 2)
#   
  # Plot reasonably well
  
  
  
############## STILL TO DO ###############
  # Set up the above formulation over all school catchments
  # Start integrating these more accurate samples with the clustering and TSP work