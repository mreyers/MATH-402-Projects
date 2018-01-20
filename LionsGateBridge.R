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



