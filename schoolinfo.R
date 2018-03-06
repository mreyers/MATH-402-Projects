#some data cleaning 
#data from 2 sources (but both from http://www.bced.gov.bc.ca/)
#1 file containing # enrolments of 2017/18 and for each grade
#seperate files for # enrolments of years before. but main interest is on 2016 since we have the 
#population density of 2016 we may need # of students of the same year
# we store 2017/18 incase we need it!




library(xlsx)


#importing the excel file containing all schools of vancouver (info intrested: #students in 2017/18)
allSchoolinfo <- read.xlsx("excelSchoolContact", 1,as.data.frame=TRUE, header=TRUE, as.is = TRUE, stringsAsFactors = FALSE)
test <- read.xlsx2("92-151-XBB_XLSX.xlsx", 1, as.data.frame=TRUE, header=TRUE, as.is = TRUE, stringsAsFactors = FALSE)


rownames(schoolinfo)
View(schoolinfo)
schoolinfo[1:10,]



###########################################################

#imporing all seperate excel files for vancouver elementary schools (info interested: #students in 2016/17)
schoolcode <- c()
enrolmentt16 <- c()


num <- as.character(c(3939028:3939143))
file <- paste0("data/0",num, ".xlsx") 

enrolment16 <- c()
range <- 1:length(file)

#saving each file, then esxtract the total number of students in 2016/17
for (i in range) {
  if (file.exists(file[i])){
    schoolpop<- read.xlsx2(file[i], sheetIndex = 1,as.data.frame=TRUE, header=TRUE, stringsAsFactors = FALSE)
    enrolment16[i] <- schoolpop$X..1[11]
  }
}

enrolment16[105]
length(num)
length(enrolment16)
schoolInformation <- cbind(paste0("0",num), enrolment16) %>% as.data.frame()
colnames(schoolInformation) <- c("schoolCode", "enrolment16")

View(schoolInformation)

schoollll <- na.omit(schoolInformation)
View(schoollll)
nrow(schoollll)


range1 <- 1:nrow(schoollll)
range2 <- 1:nrow(schoolinfo)

for (i in range1){
  for(j in range2){
    if(schoollll$schoolCode[i] == schoolinfo$School.Code[j]){
        schoollll$enrolment17[i] <- schoolinfo$Enrolment.Total[j]
        schoollll$gradeRange[i] <- schoolinfo$Grade.Range[j]
        schoollll$schoolName[i] <- schoolinfo$School.Name[j]
        schoollll$postalCode[i] <- schoolinfo$Postal.Code[j]
        break
    }
  }
}
View(schoollll)
schoollll$schoolCode[1] == schoolinfo$School.Code[91]
nrow(schoollll)

write.xlsx(schoollll, "schoolinformatoin.xlsx")
