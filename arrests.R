
setwd("U:/CityWide Performance/Police/Clearance Rates/Arrests")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")


#Read the contents of all file into a data.frame
####  ARRESTS ####
arrest  <-  read.csv(file="ArrestFY2016FromServer1.csv", header=TRUE, na.strings = c("", NA), stringsAsFactors = FALSE)



##Coding for DUI and Gun arrests
arrest$DUI <- ifelse(grepl("OPER MTR", arrest$CrimCharge),"YES", "NO")
arrest$Gun <- ifelse(grepl("HANDGUN", arrest$CrimCharge),"YES", 
                     ifelse(grepl("DEADLY WEAPON", arrest$CrimCharge),"YES", "No"))

arrest$Count <- 1

#####Trim from right the date column to remove CASE NUMBER
arrest$ArrestNumber1 <- substr(arrest$ArrestNumber1,1, nchar(arrest$ArrestNumber1)-13) # long version for triming from right
#arrest$ArrestNumber1 <- str_sub(arrest$ArrestNumber1, -50, -15) # shorter version for triming from right

##Output for CovStat Repository
names(arrest) <- c("Date", "Name","Location", "Lat", "Long", "Crime Charge", "DUI", "Gun", "Count")
arrest <- arrest[!(is.na(arrest$Date) | arrest$Date==""),]
arrest$Date <- ifelse(grepl("2013", arrest$Date), "2013", as.character(arrest$Date))
arrest <- subset(arrest, Date != "2013")


###Prepare file for tableau
arrest$Name <- NULL
arrest$Location <- NULL

#arrest <- data.frame("T"=rownames(arrest), arrest)

write.csv(arrest, file="U:/CityWide Performance/Police/Clearance Rates/Arrests/Arrests.csv")
write.csv(arrest, file="C:/Users/tsink/Mapping/Geocoding/Police/Arrests.csv")