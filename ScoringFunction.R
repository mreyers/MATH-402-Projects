
#######checking each constraint seperately.#######
#and return points that will be deducted in scoring function:

##### points : 3 important, 2 ok, 1 not very important 

library(MASS)

example <- read.csv("example_info.csv", header = T, as.is = T, stringsAsFactors = F)
str(example)
names(example)
nrow(example)

#function to split hour and minutes and convert them to numeric from character (saved as matrix)
splitHourMinute <- function(x){
  sapply(strsplit(x,":"), 
         function(x){
           x<- as.numeric(x)
         })
}

#test:
leave <- splitHourMinute(example$LeaveHome)
school <- splitHourMinute(example$SchoolTime)

#example of hour and minute students leave home: 
#leave[1,1]
#leave[1,]
#leave[2,1:15]



#funciton to adding two times (leaving home + duration time to get to school)
# leaveHomeTime = time student leaves home (matrix)
#urationTimeInMin = time takes to get to school
timeGetToSchool <- function(leaveHomeTime, durationTimeInMin){
  n <- dim(leaveHomeTime)[2]
  minute <- leaveHomeTime[2,1:n]
  addMinutes <- round(durationTimeInMin) + minute #add duration time (which is in min) 
  #to the minute that they leave home
  #handling minutes exceeding 60min (=1 hour):
  time <- matrix(1:2*n, nrow=2,ncol=n) #time get to school
  for (j in 1:n){
    if (addMinutes[j] >= 60) {
      time[1,j] <- leaveHomeTime[1,j] + 1
      time[2,j] <- addMinutes[j] - 60
    }
    else {
      time[1,j] <- leaveHomeTime[1,j]
      time[2,j] <- addMinutes[j]
    }
  }
  return(time)
}

#function to check the time student need to get to schoolconstraints 
##INPUTS:
#arrival time, get from above function, (matrix)
#schoolTime = time student has to be at school (matrix)
##OUTPUT: points
checkTime <- function(time, schoolTime){
  
  point <- c()
  for (i in 1:15){
    if (time[1,i] < schoolTime[1,i]) {point[i] <- 0}
    if (time[1,i] > schoolTime[1,i]) {point[i] <- 3}
    if (time[1,i] == schoolTime[1,i] && time[2,i] > schoolTime[2,i]){point[i] <- 3}
    if(time[1,i] == schoolTime[1,i] && time[2,i] <= schoolTime[2,i]) {point[i] <- 0}
  }
  return (point)
}

#testing: 
dur <- testTravels$studentTime
arrived <- timeGetToSchool(leave, dur)
timepoints <- checkTime(arrived, school)

#function to check the distance function
#INPUTS: 
# max = max distance student can walk
# min = min distance student should walk
#traveled = distance student actually walks
checkDistance <- function(max, min, traveled){
  acceptableExtraDistance <- 200
  n <- length(traveled)
  p <- c()
  for(i in 1:n){
    if(traveled[i] <= min[i]) {p[i] <- 0}
    if(traveled[i] > min[i] && traveled[i] <= max[i]) {p[i] <- 0}
    if(traveled[i] > min[i] && traveled[i] <= max[i] + acceptableExtraDistance){p[i] <- 1}
    if(traveled[i] > max[i] + acceptableExtraDistance){p[i] <- 3}
  }
  return (p)
}
#test: 
travelDis <- testTravels$studentDist
minDis <- example$minDistance
maxDis <- example$maxDistance
distancPoints <- checkDistance(maxDis, minDis, travelDis)


#*************need work, haven't tested yet!*****************
#function to check leader's capacity: how many students should each leader have 
#INPUTS:
#capacity= # of students each leader should have
#nstudent= # of students that a leader has already
minStudent <- 2 #minimum # of students each leader should have
checkLeadersCapacity <- function(capacity, nstudent){
  if (nstudent <= capacity) {point <- 0}
  else if (nstudent > capacity & nstudent <= capacity + 2) {point <- 1}
  else if (student > capacity+2 ) {point <- 2} #check this one if it needs to be 2!
  return (point)
}



#********* scoring funciton ******
#travel = time and distance take to school for each student
#info= data frame of file "example" 

scoreRoute <- function(travel,info, clustered){
  
  #need them?
  #leader <- clustered[clustered$leader == TRUE,]
  #nonleader <- clustered[clustered$leader != TRUE, ]
  #leaderCap <- 4   #arbitrary number change later!
  
  travelDis <- travel$studentDist
  travelTime <- travel$studentTime
  
  leaveHome <- splitHourMinute(info$LeaveHome)
  schoolTime <- splitHourMinute(info$SchoolTime)
  minDis <- info$minDistance
  maxDis <- info$maxDistance
  
  arrivalTime <- timeGetToSchool(leaveHome, travelTime)
  
  
  points<- checkTime(arrivalTime, schoolTime) + checkDistance(maxDis, minDis, travelDis)
  #nstudent <- sum(as.numeric(clustered$clusters) == i) #num of students of each cluster
  #points[i] <- points[i] + checkLeadersCapacity(leaderCap, nstudent)
  
  clustered$points <- points
  
  clusterPoints <- c()
  for (j in 1:nrow(clustered)){
    clusterPoints[j] <- sum(which(clustered$clusters == j)) 
  }
  return (clusterPoints)
  
}

#test: 

score <- scoreRoute(testTravels, example, clustered)

#notes:
#there are so many assumptions I made to just create the base of scoring funtion
#right now it just check the scores for time and distance. haven't work on leader yet!
#I assumed in "testTravels" and "clustered" students are in the same order! 
