####################################################
############ Matt's stuff, separating so I don't break current work ###############
####################################################
setwd("~/Desktop/Math 402 project")
getwd()
library(readxl)
library(XLConnect)
library(XLConnectJars)
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
allData[[132]][1]
#gives the 13th file (starting from 2005, so this is somewhere in 2006) and gets me the Neg traffic






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
         # print(row)
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

allData[[file_number(11, 2015)]][[1]]
file_number(11, 2015)

