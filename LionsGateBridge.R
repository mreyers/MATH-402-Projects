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

#Negative direction Monthly hourly volume for Feb. 2015 
Nfeb15MHV <- read.xlsx("Monthly_Hourly_Volume 02-01-2015.xls", sheetIndex = 1, rowIndex = 54:82, colIndex = 3:27)
Nfeb15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Feb 2015 
Pfeb15MHV <- read.xlsx("Monthly_Hourly_Volume 02-01-2015.xls", sheetIndex = 1, rowIndex = 97:125, colIndex = 3:27)
Pfeb15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for March 2015 
Nmar15MHV <- read.xlsx("Monthly_Hourly_Volume 03-01-2015.xls", sheetIndex = 1, rowIndex = 57:87, colIndex = 3:27)
Nmar15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for March 2015 
Pmar15MHV <- read.xlsx("Monthly_Hourly_Volume 03-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Pmar15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for Apr. 2015 
Napr15MHV <- read.xlsx("Monthly_Hourly_Volume 04-01-2015.xls", sheetIndex = 1, rowIndex = 56:86, colIndex = 3:27)
Napr15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Apr. 2015 
Papr15MHV <- read.xlsx("Monthly_Hourly_Volume 04-01-2015.xls", sheetIndex = 1, rowIndex = 101:131, colIndex = 3:27)
Papr15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for May 2015 
Nmay15MHV <- read.xlsx("Monthly_Hourly_Volume 05-01-2015.xls", sheetIndex = 1, rowIndex = 57:87, colIndex = 3:27)
Nmay15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for May 2015 
Pmay15MHV <- read.xlsx("Monthly_Hourly_Volume 05-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Pmay15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for June 2015 
Njun15MHV <- read.xlsx("Monthly_Hourly_Volume 06-01-2015.xls", sheetIndex = 1, rowIndex = 56:86, colIndex = 3:27)
Njun15MHV[29,"X22.00"]
#Positive direction Monthly hourly volume for June 2015 
Pjun15MHV <- read.xlsx("Monthly_Hourly_Volume 06-01-2015.xls", sheetIndex = 1, rowIndex = 101:131, colIndex = 3:27)
Pjun15MHV[29,"X22.00"]

#Negative direction Monthly hourly volume for July 2015 
Njul15MHV <- read.xlsx("Monthly_Hourly_Volume 07-01-2015.xls", sheetIndex = 1, rowIndex = 57:88, colIndex = 3:27)
Njul15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for July 2015 
Pjul15MHV <- read.xlsx("Monthly_Hourly_Volume 07-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Pjul15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for Aug. 2015 
Naug15MHV <- read.xlsx("Monthly_Hourly_Volume 08-01-2015.xls", sheetIndex = 1, rowIndex = 57:88, colIndex = 3:27)
Naug15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Aug. 2015 
Paug15MHV <- read.xlsx("Monthly_Hourly_Volume 08-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Paug15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for Sep. 2015 
Nsep15MHV <- read.xlsx("Monthly_Hourly_Volume 09-01-2015.xls", sheetIndex = 1, rowIndex = 56:86, colIndex = 3:27)
Nsep15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Sep. 2015 
Psep15MHV <- read.xlsx("Monthly_Hourly_Volume 09-01-2015.xls", sheetIndex = 1, rowIndex = 101:131, colIndex = 3:27)
Psep15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for Oct. 2015 
Noct15MHV <- read.xlsx("Monthly_Hourly_Volume 10-01-2015.xls", sheetIndex = 1, rowIndex = 57:88, colIndex = 3:27)
Noct15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Oct. 2015 
Poct15MHV <- read.xlsx("Monthly_Hourly_Volume 10-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Poct15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for Nov. 2015 
Nnov15MHV <- read.xlsx("Monthly_Hourly_Volume 11-01-2015.xls", sheetIndex = 1, rowIndex = 56:86, colIndex = 3:27)
Nnov15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Nov. 2015 
Pnov15MHV <- read.xlsx("Monthly_Hourly_Volume 11-01-2015.xls", sheetIndex = 1, rowIndex = 101:131, colIndex = 3:27)
Pnov15MHV[1,"X22.00"]

#Negative direction Monthly hourly volume for Dec. 2015 
Ndec15MHV <- read.xlsx("Monthly_Hourly_Volume 12-01-2015.xls", sheetIndex = 1, rowIndex = 57:88, colIndex = 3:27)
Ndec15MHV[1,"X22.00"]
#Positive direction Monthly hourly volume for Dec. 2015 
Pdec15MHV <- read.xlsx("Monthly_Hourly_Volume 12-01-2015.xls", sheetIndex = 1, rowIndex = 103:134, colIndex = 3:27)
Pdec15MHV[1,"X22.00"]
