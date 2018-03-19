
####################################################################################################
#                                        FUNCTIONS                                                  #
####################################################################################################

#check which cluster has the lowest distance and time travel 
clusterCheck <- function(c1){
  timeAccept <- 35 #30min walk
  distAccept <- 1600 #1.6 km = 1 mile walk
  
  #checking if travel distance is more than 1600, then check if they walk more than 35min
  if ((sum(c1$studentDist > distAccept)!=0) && (sum(c1$studentTime > timeAccept)!=0)){
    return(FALSE)
  }else{return(TRUE)}
}

#Function find number of students with each leader
nStudent_Leader <- function(traveldf){
  
  numClus <- max(as.numeric(traveldf$clusters))
  nstudent <- c()
  for (j in 1:numClus){
    nstudent[j] <- sum(as.numeric(traveldf$clusters) == j) #num of students of each cluster
  }
  return (nstudent)
}


#function to check leader's capacity: how many students should each leader have 
#capacity= # of students each leader should have
#nstudent= # of students that a leader has already
checkLeadersCapacity <- function(capacity, nstudent){
  pts <- c()
  for(i in 1: length(nstudent)){
    if (nstudent[i] > capacity) {
      pts[i] <- 1
    }else(pts[i] <- 0)
  }
  
  return (pts)
}

#function to score each clustering method based on how many extra students leaders have!
leaderHandling <- function(stsNum, leaderCapVector){
  
  if( sum(leaderCapVector) != 0) {
    #print(n)
    diff <- max(stsNum) - min(stsNum)
    return(diff)
  }else{return(0)}
}

#function check if for each school, how the 4 clustering method pass the "hard constraint" 
#it finds how many of them pass and how many fail
clusterMethodsDT <- function(CmethodsList){
  
  pass <- vector()
  
  for(j in 1:length(CmethodsList)){
    
    if(clusterCheck(CmethodsList[[j]])){
      pass[j] <- "p"
    }
    else{
      pass[j] <-"f"
    }
  }
  return(pass)
}  

#find distance and time voilation
howMuch <- function(c1){
  timeAccept <- 35 #30min walk
  distAccept <- 1610 #1.61 km = 1 mile walk

  #returning nodes that distance and time are voilated
  #only checking distance, bc if they exceed 1.6km they also aexceed 35min
  distVoil <- c()
  timeVoil <- c()

  for(i in 1: nrow(c1)){
    if(c1$studentDist[i] > distAccept & c1$studentTime[i] > timeAccept){
      distVoil[i] <- c1$studentDist[i] - distAccept
      timeVoil[i] <- c1$studentTime[i] - timeAccept
    }
    else {
      distVoil[i] <- 0
      timeVoil[i] <- 0
    }
  }


  return(cbind(distVoil, timeVoil)%>%as.data.frame())
}

#scoring
doScoring <- function(list, clusterMethods){
  pass <- clusterMethodsDT(list)
  numDistVoil <- c()
  maxDistVoil <- c()
  passCap <- c()
  leaderPoint <- c()
  for(j in 1:length(list)){
    n <- nStudent_Leader(list[[j]])
    leaderCap <- checkLeadersCapacity(5, n)  #capacity : 6 students acc. to WSB website
    passCap[j] <- sum(leaderCap) == 0 
    leaderPoint[j] <- leaderHandling(n, leaderCap) 
    numDistVoil[j] <- maxANDnum(howMuch(list[[j]]))[1]
    maxDistVoil[j] <- maxANDnum(howMuch(list[[j]]))[2]
  }
  result <-  cbind(clusterMethods,  "passT&D" = pass , passCap ,leaderPoint,numDistVoil, maxDistVoil) %>% as.data.frame(as.factor = T)
  return(result)
}


#find how many times distance exceeds 1.6K and what is the maximum violation
#in each clustering method
maxANDnum <- function(df){
  count <- 0
  for ( j in 1:nrow(df)){
    if(df$distVoil[j] != 0)
      count <- count+1
  }
  max <- max(df$distVoil)
  return(c(count, max))
}

#adding cluster number to routes(for routes data frame)
addClusterNum <- function(df){
  clusters <- c()
  count <- 1
  j <- 0
  for(m in 1:nrow(df)){
    if(count > nrow(df) ){
      j <- 0
      count <- 1}
    if(df$leader[m]){j <- j+1}
    clusters[m] <- j
    count <- count+1
  }
  return(clusters)
}

####################################################################################################
#                                        SCORING                                                 #
####################################################################################################


####################################################################################################
#                           R SIMULATION 4 CLUSTERING                                                #
####################################################################################################
schools <- 58
clusterMethods <- c("kNN", "kMeans", "Hier", "Fuzzy") 
routes <- read.csv("allRoutesForScoring.csv", header = T,stringsAsFactors = F)
str(routes)
View(routes)


#add cluster column to data frame
routes <- (cbind(routes, "clusters" = addClusterNum(routes)))


#scoring: 

result <- list()
for(i in 1:max(routes$Catchment)){
  cl <- list()
  test <- routes[routes$Catchment == i,]
  print(paste0("test",i))
  cl[[1]] <- test[test$ClustAlg == "kNN",]
  cl[[2]] <- test[test$ClustAlg == "kMeans",]
  cl[[3]] <- test[test$ClustAlg == "Hier",]
  cl[[4]] <- test[test$ClustAlg == "Fuzzy",]
  
  result[[i]] <-  doScoring(cl,clusterMethods)
}

####################################################################################################
#                                 VRP                                                       #
####################################################################################################
vrp<- read.csv("VRPScores.csv", header = T,stringsAsFactors = F)
View(vrp)

#scoring step:
clVRP <- list()
resultVRP <- list()
for(i in 1:max(vrp$schoolNameIndex)){
  clVRP[[1]] <- vrp[vrp$schoolNameIndex == i,]
  resultVRP[[i]] <-  doScoring(clVRP, clusterMethods = c("VRP"))
}


#combining 4 clusters and VRP
regions <- c(9, 29, 35, 55, 57, 8, 22, 34, 44, 45)

combined <- list()
for(r in 1:length(regions)){
  combined[[r]] <- rbind(result[[regions[r]]], resultVRP[[r]])
}

binding <- data.frame()
for(i in 1:10){
  binding <- rbind(binding, cbind(combined[[i]], schoolNameIndex = i))
}
write.csv(binding, "ClusterScores.csv")


#######checking each constraint seperately.#######
#and return points that will be deducted in scoring function:

#*NOTE: First run "WSB Projet" then run this. I'm using "clustered" and "testTravels" data sets here. 

##### points : 3 important, 2 ok, 1 not very important 

#level <- c(0,1,2)


# clustered <- cbind(clustered, studentTravels(routeMeasures))
# #function to split hour and minutes and convert them to numeric from character (saved as matrix)
# splitHourMinute <- function(x){
#   sapply(strsplit(x,":"), 
#          function(x){
#            x<- as.numeric(x)
#          })
# }
# 
# #test:
# leave <- splitHourMinute(example$LeaveHome)
# school <- splitHourMinute(example$SchoolTime)
# 
# #example of hour and minute students leave home: 
# #leave[1,1]
# #leave[1,]
# #leave[2,1:15]
# 
# 
# 
# 
# 
# #funciton to adding two times (leaving home + duration time to get to school)
# # leaveHomeTime = time student leaves home (matrix)
# #urationTimeInMin = time takes to get to school
# timeGetToSchool <- function(leaveHomeTime, durationTimeInMin){
#   n <- dim(leaveHomeTime)[2]
#   #get the min
#   minute <- leaveHomeTime[2,1:n]
#   #add duration time ( in min) to the minute that they leave home
#   addMinutes <- round(durationTimeInMin) + minute 
#   
#   #handling minutes exceeding 60min (=1 hour):
#   time <- matrix(1:2*n, nrow=2,ncol=n) #time get to school
#   for (j in 1:n){
#     if (addMinutes[j] >= 60) {
#       time[1,j] <- leaveHomeTime[1,j] + 1
#       time[2,j] <- addMinutes[j] - 60
#     }
#     else {
#       time[1,j] <- leaveHomeTime[1,j]
#       time[2,j] <- addMinutes[j]
#     }
#   }
#   return(time)
# }
# 
# #function to check the time student need to get to schoolconstraints 
# ##INPUTS:
# #arrival time, get from above function, (matrix)
# #schoolTime = time student has to be at school (matrix)
# ##OUTPUT: points
# checkTime <- function(time, schoolTime){
#   
#   point <- c()
#   for (i in 1:dim(schoolTime)[2]){
#     if (time[1,i] < schoolTime[1,i]) {point[i] <- level[1]}
#     if (time[1,i] > schoolTime[1,i]) {point[i] <- level[3]}
#     if (time[1,i] == schoolTime[1,i] && time[2,i] > schoolTime[2,i]){point[i] <- level[3]}
#     if(time[1,i] == schoolTime[1,i] && time[2,i] <= schoolTime[2,i]) {point[i] <- level[1]}
#   }
#   return (point)
# }
# 
# #testing: 
# dur <- clustered$studentTime
# arrived <- timeGetToSchool(leave, dur)
# timepoints <- checkTime(arrived, school)
# 
# #function to check the distance function
# #INPUTS: 
# # max = max distance student can walk
# # min = min distance student should walk
# #traveled = distance student actually walks
# checkDistance <- function(max, min, traveled){
#   acceptableExtraDistance <- 200
#   n <- length(traveled)
#   p <- c()
#   for(i in 1:n){
#     if(traveled[i] <= min[i]) {p[i] <- level[1]}
#     if(traveled[i] > min[i] && traveled[i] <= max[i]) {p[i] <- level[1]}
#     if(traveled[i] > min[i] && traveled[i] <= max[i] + acceptableExtraDistance){p[i] <- level[2]}
#     if(traveled[i] > max[i] + acceptableExtraDistance){p[i] <- level[3]}
#   }
#   return (p)
# }
# 
# #test: 
# travelDis <- clustered$studentDist
# minDis <- example$minDistance
# maxDis <- example$maxDistance
# distancPoints <- checkDistance(maxDis, minDis, travelDis)
# 


#********* scoring funciton ******
#travel = time and distance take to school for each student
#info= data frame of file "example" 

# scoreRoute <- function(traveldf,info){
#   
#   
#   range <- 1: max(as.numeric(traveldf$clusters))
#   
#   studentNumPerLeader <- nStudent_Leader(traveldf)
# 
#   #################################################################  
#   leaderCap <- 5   #arbitrary number for leader Capacity!        # PLACE THIS SOMEWHERE ELSE!
#   ################################################################
# #   
#   leaveHome <- splitHourMinute(info$LeaveHome)
#   schoolTime <- splitHourMinute(info$SchoolTime)
#   arrivalTime <- timeGetToSchool(leaveHome, traveldf$studentTime)
#   
#   timePoint <- checkTime(arrivalTime, schoolTime)
#   distancePoint <- checkDistance(info$maxDistance, info$minDistance, traveldf$studentDist)
#   
#   df <- cbind(traveldf[ , !(names(traveldf) %in% c("studentDist","studentTime"))], timePoint, distancePoint) %>% as.data.frame()
#   
#   df$leaderPoint <- 0
#   for(k in range){
#     df$leaderPoint[df$leader==TRUE][k] <- checkLeadersCapacity(leaderCap, studentNumPerLeader)[k]
#   }
#   
#   #total points for each node
#   df$pointsTot <- rowSums(df[, c("leaderPoint","timePoint", "distancePoint")])
#   
#   return (df)
# }
# 
# #test: 
# score <- scoreRoute(clustered, example)
# View(score)


# #function make a list of each cluster with individual points for each student in detail
# #INPUT : DATA FRAME 
# #OUTPUT : LIST
# groupByCluster <- function(scoredf){
#   
#   clus <- list()
#   
#   for (i in 1: max(as.numeric(scoredf$clusters)) ){
#   clus[[i]] <- scoredf[scoredf$clusters == i, ]
#   }
#   return(clus)
# }
# 
# #test:
# groupByCluster(score)



# function to give the total score for each cluster
# INPUT: DATA FRAME (SCORE OF EACH STUDENT)
# #OUTPUT: DATA FRAME (TOTAL SCORE)
# clusterTotalScore <- function(scoredf) {
#   
#   cluster <- 1:max(as.numeric(scoredf$clusters))
#   totalPoints <- c()
#   
#   for(i in cluster){
#     totalPoints[i] <- sum(scoredf$pointsTot[scoredf$clusters == i])
#   }
#   
#   css <- cbind(cluster, totalPoints) %>% as.data.frame()
#   return(css)
# }
# 
# #test:
# clusterTotalScore(score)

# #find the one with the lowest violation
# ttp <- c()
# if(length(passed) == 0){
#   for(i in 1:length(notPassed)){
#     ttp[i] <- sum(scoreTravel(allMeasures[[1]][[notPassed[i]]])$timePoint)
#   }
#   chosenClust <- which(ttp == min(ttp))
# }else{
#   for(i in 1:length(passed)){
#     print(passed[i])
#     #for now we just have leader points besides theother two!
#     ttp[i] <- sum(addLeaderPoint(allMeasures[[2]][[passed[i]]],5)$leaderPoint)
#   }
#   chosenClust <- passed[which(ttp == min(ttp))]
# 
# }






#adding leader points to the data frame of all nodes
# addLeaderPoint <- function(df,leaderCap){
#   ss <- nStudent_Leader(df)
#   df$leaderPoint <- 0
#   for(k in 1:nrow(df)){
#     if(df$leader[k]){
#     df$leaderPoint[k] <- checkLeadersCapacity(leaderCap, ss)[m]
#     m <- m+1
#   }
#  }
#   return(df)
# }
# addLeaderPoint(allMeasures[[2]][[1]],3)



# scoreTravel <- function(c1){
#   
#   #later pass this to the function?
#   acceptableExtraDistance <- 200
#   acceptableExtraTime <- 5
#   
#   
#   df <- howMuch(c1)
#   distPoint <- c()
#   timePoint <- c()
#   
#   #distance point:
#   for (i in 1: nrow(df)) {
#     if(df$distVoil[i] == 0){distPoint[i] <- level[1]}
#     else {distPoint[i] <- level[2]} 
#     
#     # if(df$distVoil[i] > acceptableExtraDistance){
#     #   distPoint[i] <- distPoint[i] +  level[2]}
#     # 
#   }
#   
#   #time point:
#   for (i in 1: nrow(df)) {
#     if(df$timeVoil[i] == 0){timePoint[i] <- level[1]}
#     else {timePoint[i] <- level[2]} 
#     
#     # if(df$timeVoil[i] > acceptableExtraTime){
#     #   timePoint[i] <- timePoint[i] +  level[2]}
#     # 
#     
#   }
#   return(cbind(c1, distPoint, timePoint))
# }
# 
# sum(scoreTravel(allMeasures[[1]][[2]])$distPoint)



# 
# #if getting "FALSE" in clusterCheck and want to find out how many routes violate the
# #distance and time and which routes are they.
# #INPUT: data frame 
# whichroute <- function(c1){
#   
#   timeAccept <- 35 #30min walk
#   distAccept <- 1610 #1.61 km = 1 mile walk
#   
#   #returning nodes that distance and time are voilated
#   #only checking distance, bc if they exceed 1.6km they also aexceed 35min
#   c <- c1 %>% filter(studentDist > distAccept & studentTime > timeAccept) 
#   return(c)
# }
# 
# #find distance and time voilation
# howMuch <- function(c1){
#   timeAccept <- 35 #30min walk
#   distAccept <- 1610 #1.61 km = 1 mile walk
#   
#   #returning nodes that distance and time are voilated
#   #only checking distance, bc if they exceed 1.6km they also aexceed 35min
#   distVoil <- c()
#   timeVoil <- c()
#   
#   for(i in 1: nrow(c1)){
#     if(c1$studentDist[i] > distAccept & c1$studentTime[i] > timeAccept){
#       distVoil[i] <- c1$studentDist[i] - distAccept
#       timeVoil[i] <- c1$studentTime[i] - timeAccept
#     }
#     else {
#       distVoil[i] <- 0
#       timeVoil[i] <- 0
#     }
#   }
#   
#   
#   return(cbind(c1, distVoil, timeVoil))
# }
# 



