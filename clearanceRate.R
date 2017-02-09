
setwd("U:/CityWide Performance/CovStat/CovStat Projects/Police/Part I & Part II/ClearanceRates")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")
library("RSQLite")

### Connect and Load Database ####

cons.police <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Police.db")
cleareance <- dbGetQuery(cons.police, 'select * from ClearanceRates')


#Load Update File
#Change in format of data
update  <-  read.csv(file="NEWFormat_January17.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
update <- data.frame(update, ind=rep(1:2, nrow(update)/2))
updateV1 <- unstack(update, V1~ind)
updateV2 <- unstack(update, V2~ind)
update <- do.call("cbind", list(updateV1, updateV2))
names(update)[1] <- "Charge.Code"
names(update)[2] <- "Count"
names(update)[3] <- "Charge"
names(update)[4] <- "C_rate"
update <- update[c("C_rate", "Count", "Charge", "Charge.Code")]
update$Month <- "January"
update$Year <- "2017"
update$Date <- "01/31/2017"


#January15  <-  read.csv(file="January15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
#January15 <- data.frame(January15, ind=rep(1:4, nrow(January15)/4))
#January15 <- unstack(January15, V1~ind)
#names(January15) <- c("C_rate", "Count", "Charge", "Charge.Code")
#January15$Month <- "January"
#January15$Year <- "2015"
#January15$Date <- "01/31/2015"





dbWriteTable(cons.police, "ClearanceRates", combined, overwrite = TRUE)
dbDisconnect(cons.police)













January15  <-  read.csv(file="January15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
January15 <- data.frame(January15, ind=rep(1:4, nrow(January15)/4))
January15 <- unstack(January15, V1~ind)
names(January15) <- c("C_rate", "Count", "Charge", "Charge.Code")
January15$Month <- "January"
January15$Year <- "2015"
January15$Date <- "01/31/2015"

February15  <-  read.csv(file="February15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
February15 <- data.frame(February15, ind=rep(1:4, nrow(February15)/4))
February15 <- unstack(February15, V1~ind)
names(February15) <- c("C_rate", "Count", "Charge", "Charge.Code")
February15$Month <- "February"
February15$Year <- "2015"
February15$Date <- "02/28/2015"

March15  <-  read.csv(file="March15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
March15 <- data.frame(March15, ind=rep(1:4, nrow(March15)/4))
March15 <- unstack(March15, V1~ind)
names(March15) <- c("C_rate", "Count", "Charge", "Charge.Code")
March15$Month <- "March"
March15$Year <- "2015"
March15$Date <- "03/31/2015"

April15  <-  read.csv(file="April15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
April15 <- data.frame(April15, ind=rep(1:4, nrow(April15)/4))
April15 <- unstack(April15, V1~ind)
names(April15) <- c("C_rate", "Count", "Charge", "Charge.Code")
April15$Month <- "April"
April15$Year <- "2015"
April15$Date <- "04/30/2015"

May15  <-  read.csv(file="May15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
May15 <- data.frame(May15, ind=rep(1:4, nrow(May15)/4))
May15 <- unstack(May15, V1~ind)
names(May15) <- c("C_rate", "Count", "Charge", "Charge.Code")
May15$Month <- "May"
May15$Year <- "2015"
May15$Date <- "05/31/2015"

June15  <-  read.csv(file="June15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
June15 <- data.frame(June15, ind=rep(1:4, nrow(June15)/4))
June15 <- unstack(June15, V1~ind)
names(June15) <- c("C_rate", "Count", "Charge", "Charge.Code")
June15$Month <- "June"
June15$Year <- "2015"
June15$Date <- "06/30/2015"

July15  <-  read.csv(file="July15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
July15 <- data.frame(July15, ind=rep(1:4, nrow(July15)/4))
July15 <- unstack(July15, V1~ind)
names(July15) <- c("C_rate", "Count", "Charge", "Charge.Code")
July15$Month <- "July"
July15$Year <- "2015"
July15$Date <- "07/31/2015"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
August15  <-  read.csv(file="August15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
August15 <- data.frame(August15, ind=rep(1:4, nrow(August15)/4))
August15 <- unstack(August15, V1~ind)
names(August15) <- c("C_rate", "Count", "Charge", "Charge.Code")
August15$Month <- "August"
August15$Year <- "2015"
August15$Date <- "08/31/2015"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
September15  <-  read.csv(file="September15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
September15 <- data.frame(September15, ind=rep(1:4, nrow(September15)/4))
September15 <- unstack(September15, V1~ind)
names(September15) <- c("C_rate", "Count", "Charge", "Charge.Code")
September15$Month <- "September"
September15$Year <- "2015"
September15$Date <- "09/30/2015"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
October15  <-  read.csv(file="October15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
October15 <- data.frame(October15, ind=rep(1:4, nrow(October15)/4))
October15 <- unstack(October15, V1~ind)
names(October15) <- c("C_rate", "Count", "Charge", "Charge.Code")
October15$Month <- "October"
October15$Year <- "2015"
October15$Date <- "10/31/2015"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
November15  <-  read.csv(file="November15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
November15 <- data.frame(November15, ind=rep(1:4, nrow(November15)/4))
November15 <- unstack(November15, V1~ind)
names(November15) <- c("C_rate", "Count", "Charge", "Charge.Code")
November15$Month <- "November"
November15$Year <- "2015"
November15$Date <- "11/30/2015"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
December15  <-  read.csv(file="December15.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
December15 <- data.frame(December15, ind=rep(1:4, nrow(December15)/4))
December15 <- unstack(December15, V1~ind)
names(December15) <- c("C_rate", "Count", "Charge", "Charge.Code")
December15$Month <- "December"
December15$Year <- "2015"
December15$Date <- "12/31/2015"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
January16  <-  read.csv(file="January16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
January16 <- data.frame(January16, ind=rep(1:4, nrow(January16)/4))
January16 <- unstack(January16, V1~ind)
names(January16) <- c("C_rate", "Count", "Charge", "Charge.Code")
January16$Month <- "January"
January16$Year <- "2016"
January16$Date <- "01/31/2016"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
February16  <-  read.csv(file="February16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
February16 <- data.frame(February16, ind=rep(1:4, nrow(February16)/4))
February16 <- unstack(February16, V1~ind)
names(February16) <- c("C_rate", "Count", "Charge", "Charge.Code")
February16$Month <- "February"
February16$Year <- "2016"
February16$Date <- "02/29/2016"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
March16  <-  read.csv(file="March16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
March16 <- data.frame(March16, ind=rep(1:4, nrow(March16)/4))
March16 <- unstack(March16, V1~ind)
names(March16) <- c("C_rate", "Count", "Charge", "Charge.Code")
March16$Month <- "March"
March16$Year <- "2016"
March16$Date <- "03/31/2016"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
April16  <-  read.csv(file="April16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
April16 <- data.frame(April16, ind=rep(1:4, nrow(April16)/4))
April16 <- unstack(April16, V1~ind)
names(April16) <- c("C_rate", "Count", "Charge", "Charge.Code")
April16$Month <- "April"
April16$Year <- "2016"
April16$Date <- "04/30/2016"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
May16  <-  read.csv(file="May16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
May16 <- data.frame(May16, ind=rep(1:4, nrow(May16)/4))
May16 <- unstack(May16, V1~ind)
names(May16) <- c("C_rate", "Count", "Charge", "Charge.Code")
May16$Month <- "May"
May16$Year <- "2016"
May16$Date <- "05/31/2016"

#Read the contents of all file into a data.frame. Unstacking clearance report from single column.  split to new row at every 4th row.
June16  <-  read.csv(file="June16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
June16 <- data.frame(June16, ind=rep(1:4, nrow(June16)/4))
June16 <- unstack(June16, V1~ind)
names(June16) <- c("C_rate", "Count", "Charge", "Charge.Code")
June16$Month <- "June"
June16$Year <- "2016"
June16$Date <- "06/30/2016"

July16  <-  read.csv(file="July16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
July16 <- data.frame(July16, ind=rep(1:4, nrow(July16)/4))
July16 <- unstack(July16, V1~ind)
names(July16) <- c("C_rate", "Count", "Charge", "Charge.Code")
July16$Month <- "July"
July16$Year <- "2016"
July16$Date <- "07/31/2016"

August16  <-  read.csv(file="August16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
August16 <- data.frame(August16, ind=rep(1:4, nrow(August16)/4))
August16 <- unstack(August16, V1~ind)
names(August16) <- c("C_rate", "Count", "Charge", "Charge.Code")
August16$Month <- "August"
August16$Year <- "2016"
August16$Date <- "08/31/2016"

September16  <-  read.csv(file="September16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
September16 <- data.frame(September16, ind=rep(1:4, nrow(September16)/4))
September16 <- unstack(September16, V1~ind)
names(September16) <- c("C_rate", "Count", "Charge", "Charge.Code")
September16$Month <- "September"
September16$Year <- "2016"
September16$Date <- "09/30/2016"

##Bind data frames together
combined <- do.call("rbind", list(January15, February15, March15, April15, May15, June15, July15,August15,September15, October15, November15, 
                                  December15, January16, February16, March16, April16,
                                  May16, June16, July16, August16, September16))


#Change in format of data
October16  <-  read.csv(file="NEWFormat_October16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
October16 <- data.frame(October16, ind=rep(1:2, nrow(October16)/2))
October16V1 <- unstack(October16, V1~ind)
October16V2 <- unstack(October16, V2~ind)
October16 <- do.call("cbind", list(October16V1, October16V2))
names(October16)[1] <- "Charge.Code"
names(October16)[2] <- "Count"
names(October16)[3] <- "Charge"
names(October16)[4] <- "C_rate"
October16 <- October16[c("C_rate", "Count", "Charge", "Charge.Code")]
October16$Month <- "October"
October16$Year <- "2016"
October16$Date <- "10/31/2016"

#Change in format of data
November16  <-  read.csv(file="NEWFormat_November16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
November16 <- data.frame(November16, ind=rep(1:2, nrow(November16)/2))
November16V1 <- unstack(November16, V1~ind)
November16V2 <- unstack(November16, V2~ind)
November16 <- do.call("cbind", list(November16V1, November16V2))
names(November16)[1] <- "Charge.Code"
names(November16)[2] <- "Count"
names(November16)[3] <- "Charge"
names(November16)[4] <- "C_rate"
November16 <- November16[c("C_rate", "Count", "Charge", "Charge.Code")]
November16$Month <- "November"
November16$Year <- "2016"
November16$Date <- "11/30/2016"

#Change in format of data
December16  <-  read.csv(file="NEWFormat_December16.csv", header=FALSE, na.strings = "NA", stringsAsFactors = FALSE)
December16 <- data.frame(December16, ind=rep(1:2, nrow(December16)/2))
December16V1 <- unstack(December16, V1~ind)
December16V2 <- unstack(December16, V2~ind)
December16 <- do.call("cbind", list(December16V1, December16V2))
names(December16)[1] <- "Charge.Code"
names(December16)[2] <- "Count"
names(December16)[3] <- "Charge"
names(December16)[4] <- "C_rate"
December16 <- December16[c("C_rate", "Count", "Charge", "Charge.Code")]
December16$Month <- "December"
December16$Year <- "2016"
December16$Date <- "12/31/2016"

combined <- do.call("rbind", list(combined, October16, November16, December16))


#If 'Quantity' is not the number of crimes than work backwards using percent cleared and quantity of crimes cleared to get number of crimes
#Will need to change to numeric class, trim % symbol, create new column showing percent as ratio (Eg. 85% to .85), and perfrom calculation to get number of crimes
combined$C_rate <- substr(combined$C_rate,1, nchar(combined$C_rate)-1)
combined$C_rate <- as.numeric(combined$C_rate)
combined$Count <- as.numeric(combined$Count)
combined$pct_ratio <- combined$C_rate *.01
combined$CrimesCleared <- combined$pct_ratio * combined$Count



#combined$C_rate <- str_replace_all(combined$C_rate, "[%]", "")
#combined$C_rate <- as.numeric(combined$C_rate)
#combined$Count <- as.numeric(combined$Count)

###Adjustment to charge names "ALL OTHER OFFENSES (EXCEPT TRAFFIC)" based on charge code to allow for proper sorting in Tableau

#combined$Charge[combined$Charge.Code == "26C" | combined$Charge.Code == "100" | combined$Charge.Code == "26G"  
               # | combined$Charge.Code == "280" | combined$Charge.Code == "720"] <- "ALL OTHER OFFENSES (EXCEPT TRAFFIC)a"


#combined$Charge[combined$Charge.Code == "90Z" | combined$Charge.Code == "90J" ] <- "ALL OTHER OFFENSES (EXCEPT TRAFFIC)b" 

write.csv(combined, file="U:/CityWide Performance/Police/Part I & Part II/ClearanceRates/ClearanceRates.csv")

##Write to CovStat Repository
write.csv(combined, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Police/ClearanceRates.csv")

save(combined, file="U:/CityWide Performance/Police/Part I & Part II/ClearanceRates/clearanceRates.RData")


### Subsetting by Date ###
#test <- port.ins[port.ins$NewLeaseDate > as.Date("1899-12-31"),]

#as.Date(x, origin = "1970-01-01")

######################################
###  Connect and write to SQLite  ###










