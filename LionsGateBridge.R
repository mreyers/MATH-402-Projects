#Lions Gate Bridge
#Idea: 
#     Data will be used from:
#     http://www.th.gov.bc.ca/trafficdata/tradas/tradas.asp?loc=P-15-1NS

setwd("~/Desktop/Math 402 project")
getwd()

library(xlsx)
library(readxl)

#Total Monthly hourly volume for january 2015 
Tjan15MHV <- read.xlsx("Monthly_Hourly_Volume 01-01-2015.xls", sheetIndex = 1, rowIndex = 11:42, colIndex = 3:27)
Tjan15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for january 2015 
Njan15MHV <- read.xlsx("Monthly_Hourly_Volume 01-01-2015.xls", sheetIndex = 1, rowIndex = 57:88, colIndex = 3:27)
Njan15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for january 2015 
Pjan15MHV <- read.xlsx("Monthly_Hourly_Volume 01-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Pjan15MHV[1,"X22.00"]
#Njan15MHV+Pjan15MHV


#test = read.xlsx("Monthly_Hourly_Volume 01-01-2015.xls", sheetIndex=1)
#firstcol = as.character(test[,3])
#rowstarts = which(firstcol == "0:00")

#rows1 = rowstarts[1] + 1
#rows2 = rowstarts[2] + 1
#rows3 = rowstarts[3] + 1

#rowsStart = rowstarts + 1
#End = as.character(test[,2])

#End = which(End == "31")

#months = rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),times=13)
#years = rep(2003:2015,each=12)
#filenames = paste0("Monthly_Hourly_Volume ",months,"-01-",years,".xls")

month = c("01","02","03","04","05","06","07","08","09","10","11","12")
yearini2015 = paste0("Monthly_Hourly_Volume ",month,"-01-2015.xls")

#storing all months of 2015 in a list 
range  = 1:12
year2015 = list()
for (i in range) {
  year2015[[i]]<- read.xlsx(yearini2015[i], sheetIndex = 1)
}

str(year2015)
View(year2015[[1]])

#finding the first and last row for each part (roadway, negative and positive ways)
rowsStart = list()
rowsEnd = list()
for (i in 1:12){
  firstcol = as.character(year2015[[i]][,3])
  rowsStart[[i]] = which(firstcol == "0:00")
  rowsStart[[i]] = rowsStart[[i]]+1
  firstcol = as.character(year2015[[i]][,3])
  rowsEnd[[i]] = rowsStart[[i]]+30
}
firstcol = as.character(year2015[[1]][,3])


#test for extracting each part
cols = 3:27
roadway1 = year2015[[1]][rowsStart[[1]][1]:rowsEnd[[1]][1],cols] 

colnames(roadway1) <- c("midnight", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am", 
                        "11 am", "noon", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", 
                        "11 pm", "Total" )
View(roadway1)

#initialize 3 parts
roadway15 = list()
neg15 = list()
pos15 = list()

for (i in range){
  roadway15[[i]] = year2015[[i]][rowsStart[[i]][1]:rowsEnd[[i]][1],cols]
  colnames(roadway15[[i]]) <- c("midnight", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am", 
                          "11 am", "noon", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", 
                          "11 pm", "Total" )
  neg15[[i]] = year2015[[i]][rowsStart[[i]][2]:rowsEnd[[i]][2],cols]
  colnames(neg15[[i]]) <- c("midnight", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am", 
                            "11 am", "noon", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", 
                            "11 pm", "Total" )
  pos15[[i]] = year2015[[i]][rowsStart[[i]][3]:rowsEnd[[i]][3],cols]
  colnames(pos15[[i]]) <- c("midnight", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am", 
                            "11 am", "noon", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", 
                            "11 pm", "Total" )
} 

####################################################
############ Matt's stuff, separating so I don't break current work ###############
####################################################

library(readxl)
library(tidyverse)

# Need to create a function to read in and process into appropriate files
# Cannot use base read_xlsx with file, needs special opening or renaming

loadAndSplit <- function(filename){
    # bring in the file
    data <- loadWorkbook(filename)
    test <- readWorksheet(data, "sheet1")
    
    # Only need data from certain columns and rows
    cols <- 3:27
    firstcol <- as.character(test$Col3)
    rowsStart <- which(firstcol == "0:00") + 1
    rowsEnd <- rowsStart + 31
    
    # Split the document by these cut off points
    allTraffic <- test[rowsStart[1]:rowsEnd[1], cols] 
    negDir <- test[rowsStart[2]:rowsEnd[2], cols]
    posDir <- test[rowsStart[3]:rowsEnd[3], cols]
    
    # Return a list of the 3: first all, second neg, third pos
    return(list(allTraffic, negDir, posDir))
}


# Pass a data frame to be cleaned and return the data frame
cleaner <- function(df){
  # Convert to numeric and remove unnecessary rows (as months differ in length)
  df <- as.data.frame(apply(df, MARGIN = 2, function(x) as.numeric(gsub(",", "", x)))) %>% filter(!is.na(Col4))
  # Rename columns to accurate times
  colName <- c("12 am", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am", 
               "11 am", "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", 
               "11 pm", "Total" )
  colnames(df) <- colName
  
  return(df)
}

# Example usage
holder <- loadAndSplit("Bridge Data/MV03 - Site Lions Gate - P-15-1NS - N on 01-01-2005.xls")
cleanedUp <- cleaner(holder[[1]])
head(cleanedUp)

# Create a loop to read in all the data and clean it up
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
years <- as.character(2006:2016)
allData <- list()
for(i in months){
  for( j in years){
    file_name_1 <- paste0("Bridge Data/MV03 - Site Lions Gate - P-15-1NS - N on ", i, "-01-", j, ".xls")
    file_name_2 <- paste0("Bridge Data/MV03 - Site Lions Gate P-15-1NS - NY on ", i, "-01-", j, ".xls")
    counter <- 0
    if(file.exists(file_name_1)){
      #print("File1")
      temp <- loadAndSplit(file_name_1)
      counter <- 1
    }
    else if(file.exists(file_name_2)){
      #print("File2")
      temp <- loadAndSplit(file_name_2)
      counter <- 1
    }
    # To be efficient, save at start of list. Done by basic transform
    if(counter == 1){
      row <- as.numeric(j) %% as.numeric(years[1])
      col <- as.numeric(i)
      #print(row)
      #print(col)
      allData[[row*12 + col]] <- temp
      allData[[row*12 + col]][[1]] <- allData[[row*12 + col]][[1]] %>% cleaner()
      allData[[row*12 + col]][[2]] <- allData[[row*12 + col]][[2]] %>% cleaner()
      allData[[row*12 + col]][[3]] <- allData[[row*12 + col]][[3]] %>% cleaner()
    }
    
  }
}
# Warnings generated are NA coercion, fine as we omit NAs


# AllData now contains desired data sets, access desired date in order according to following:
# AllData[[file# in order]][[1 = Total, 2 = Neg, 3 = Pos]]
  # file# = (1 first file, 2 second file, 3... in chronological order)
# Sample: AllData[[13]][2] gives the 13th file (starting from 2005, so this is somewhere in 2006) and gets me the Neg traffic

#################### API WORK BUT ITS A LITTLE OUT OF ORDER, WORKS THOUGH #########################
# API Work will be for the actual project to give better results w.r.t congestion
  # Busses of interest for Lions Gate bridge are the 240 and 250
  # Set up the API calls for each of these routes and stops
    # Bus stops:
      # City side: 51475 (routes 240, 246, 250)
      # North Side: 54440 (routes 240, 246), 54411 (route 250)
routes <- as.character(c(240, 246, 250))
stops <- as.character(c(51475, 54440, 54411))

# Structure of API call: http://api.translink.ca/rttiapi/v1/buses?apikey=[key]&stopNo=stops&routeNo=routes
  # From this I have to extract the longitude and latitude

# Data is XML
library(XML)
key <- "BVGkIJET0Q9WEvvmvQrq"

# Make a function that will be used in automated server pinging
rttiAPICaller <- function(key, stop, route){
  URL <- paste0("http://api.translink.ca/rttiapi/v1/buses?apikey=", key, "&stopNo=", stop, "&routeNo=", route)
  Output <- xmlTreeParse(URL)
  testXML <- xmlRoot(Output)
  # Loop the vehicles so we unlist the desired number (some hours there are 5 busses, others there are 2)
  for(i in 1:length(testXML)){
    if(i == 1){
      holder <- unlist(testXML[[1]])
    }
    else{
      holder <- holder %>% rbind(unlist(testXML[[i]]))
    }
  }
  if(!exists("holder") | is.null(holder)){
    # No busses running at this time
    result <- data.frame()
    result$VehicleNum <- NA
    result$Direction <- NA
    result$Lat <- NA
    result$Long <- NA
    result$Time <- NA
    result$Date <- Sys.Date()
    return(result)
  }
  else{
  # Rename from XML naming scheme and select only requisite variables
  holder <- holder %>% as.data.frame(row.names = 1:dim(holder)[1]) %>% select(children.VehicleNo.children.text.value,
                                                  children.Direction.children.text.value,
                                                  children.Latitude.children.text.value,
                                                  children.Longitude.children.text.value,
                                                  children.RecordedTime.children.text.value)
  names(holder) <- c("VehicleNum", "Direction", "Lat", "Long", "Time") 
  holder$Date <- Sys.Date()
  return(holder)
  }
}
# Valid Origin and destination: Vancouver, Taylor Way, Capilano Road
googleAPICaller <- function(key, origin, dest){
  VanLocation <- c(49.290616, -123.130654)
  TaylorWay   <- c(49.336705, -123.134838)
  CapilanoRD  <- c(49.331790, -123.115065)
  if(origin == "Vancouver" & dest == "Taylor Way"){
    VanToTaylor <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=", VanLocation[1], ",", VanLocation[2], "&destinations=", TaylorWay[1], ",", TaylorWay[2], "&key=", key)
    data <- fromJSON(VanToTaylor)
  }
  else if(origin == "Vancouver" & dest == "Capilano Road"){
    VanToCap    <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=", VanLocation[1], ",", VanLocation[2], "&destinations=", CapilanoRD[1], ",", CapilanoRD[2], "&key=", key)
    data <- fromJSON(VanToCap)
  }
  else if(origin == "Taylor Way" & dest == "Vancouver"){
    TaylorToVan <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=", TaylorWay[1], ",", TaylorWay[2], "&destinations=", VanLocation[1], ",", VanLocation[2], "&key=", key)
    data <- fromJSON(TaylorToVan)
  }
  else if(origin == "Capilano Road" & dest == "Vancouver"){
    CapToVan    <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=", CapilanoRD[1], ",", CapilanoRD[2], "&destinations=", VanLocation[1], ",", VanLocation[2], "&key=", key)
    data <- fromJSON(CapToVan)
  }
  data <- data$rows$elements[[1]]$duration$value # returns the seconds length of the trip estimate instead of bulky json data
  return(data)
}

googleIterator <- function(key){
  # DOnt need sleep function, will be pseudo done by the other function calling translink
  VanToTaylor <- googleAPICaller(key, "Vancouver", "Taylor Way")
  VanToCap    <- googleAPICaller(key, "Vancouver", "Capilano Road")
  TaylorToVan <- googleAPICaller(key, "Taylor Way", "Vancouver")
  CapToVan    <- googleAPICaller(key, "Capilano Road", "Vancouver")
  return(list(VanToTaylor, VanToCap, TaylorToVan, CapToVan))
}

iterator <- function(key){
  Sys.sleep(150) # Sleep for 2.5 minutes so data can refresh on server
  R240S51475 <- rttiAPICaller(key, "51475", "240")
  R246S51475 <- rttiAPICaller(key, "51475", "246")
  R250S51475 <- rttiAPICaller(key, "51475", "250")
  R240S54440 <- rttiAPICaller(key, "54440", "240")
  R246S54440 <- rttiAPICaller(key, "54440", "246")
  R250S54411 <- rttiAPICaller(key, "54411", "250")
  return(list(R240S51475,
              R246S51475,
              R250S51475,
              R240S54440,
              R246S54440,
              R250S54411))
}

############################
##### CALLING THE ITERATOR #####
############################
library(jsonlite)
googleKey <- "AIzaSyAZt5UyUNZVt9xxhppR2dtEsxqy-AkW1N4"
testGoogle <- googleAPICaller(googleKey, "Vancouver", "Capilano Road")


iter = 1
R240S51475 <- list() # Van to NVan
R246S51475 <- list() # Van to NVan
R250S51475 <- list() # Van to NVan
R240S54440 <- list() # NVan to Van
R246S54440 <- list() # NVan to Van
R250S54411 <- list() # NVan to Van
VanToTaylor<- data.frame() # Non-transit stuff, names are self explanatory
VanToCap   <- data.frame()
TaylorToVan<- data.frame()
CapToVan   <- data.frame()

j <- 0
while( iter < 3){
  tempResult <- iterator("BVGkIJET0Q9WEvvmvQrq")
  R240S51475[[iter]] <- tempResult[1]
  R246S51475[[iter]] <- tempResult[2]
  R250S51475[[iter]] <- tempResult[3]
  R240S54440[[iter]] <- tempResult[4]
  R246S54440[[iter]] <- tempResult[5]
  R250S54411[[iter]] <- tempResult[6]
  iter = iter + 1
  j <- j + 1
  if(j == 2){
    # Call google API half as frequently
    tempGoogleResult <- googleIterator("AIzaSyAZt5UyUNZVt9xxhppR2dtEsxqy-AkW1N4")
    VanToTaylor[iter %/% 2, 1] <- tempGoogleResult[1] 
    VanToTaylor[iter %/% 2, 2] <- as.character(Sys.time())
    VanToCap[iter %/% 2, 1] <- tempGoogleResult[2]
    VanToCap[iter %/% 2, 2] <- as.character(Sys.time())
    TaylorToVan[iter %/% 2, 1] <- tempGoogleResult[3] 
    TaylorToVan[iter %/% 2, 2] <- as.character(Sys.time())
    CapToVan[iter %/% 2, 1] <- tempGoogleResult[4]
    CapToVan[iter %/% 2, 1] <- as.character(Sys.time())
    j <- 0
  }
}


# Other API call: Real time data
  # This one has a security error so setup the loop for the first one then this
#secondTestURL <- paste0("https://rtdsapi.translink.ca/rtdsapi/v1/LiveDataAtPoint?apikey=", key, "&x=-123.04550170898438&y=49.23194729854554&z=12&types=6")
#testOutput2 <- xmlTreeParse(secondTestURL)
#testXML2 <- xmlRoot(testOutput2)

########################################################################

# Assume for now it is possible to estimate congestion via API for any given point in the day
# Above code is proof of concept that the API can be accessed
# Time and position data, in combo with maps API, will allow us to calculate average speed and therefore a measure of congestion
# Data design is feasible, question remaining is how our optimization will work
# Discuss with Tamon

#A function that returns the file number for a specific year and a month (based on ur code)
filenum <- 0
file_number <- function(month, year){
  for(i in months){
    for( j in years){
      if (as.numeric(i) == month & as.numeric(j) == year){
        #print(month)
        
        file_name_1 <- paste0("Bridge Data/MV03 - Site Lions Gate - P-15-1NS - N on ", i, "-01-", j, ".xls")
        file_name_2 <- paste0("Bridge Data/MV03 - Site Lions Gate P-15-1NS - NY on ", i, "-01-", j, ".xls")
        counter <- 0
        if(file.exists(file_name_1)){counter <- 1}
        else if(file.exists(file_name_2)){counter <- 1}
        
        if(counter == 1){
          #print(row)
          #print(col)
          row <- as.numeric(j) %% as.numeric(years[1])
          col <- as.numeric(i)
          filenum <- row*12 + col
          #print(filenum)
          #print("return here:")
          return(filenum)
        } 
      }
    }
    
  }
  
}
