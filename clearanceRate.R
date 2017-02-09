
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
clearance <- dbGetQuery(cons.police, 'select * from ClearanceRates')

####Load Update File####
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

#If 'Quantity' is not the number of crimes than work backwards using percent cleared and quantity of crimes cleared to get number of crimes
#Will need to change to numeric class, trim % symbol, create new column showing percent as ratio (Eg. 85% to .85), and perfrom calculation to get number of crimes
update$C_rate <- substr(update$C_rate,1, nchar(update$C_rate)-1)
update$C_rate <- as.numeric(update$C_rate)
update$Count <- as.numeric(update$Count)
update$pct_ratio <- update$C_rate *.01
update$CrimesCleared <- update$pct_ratio * update$Count

####Bind database and update####
update <- do.call("rbind", list(update, clearance))

####Overwrite Database####
dbWriteTable(cons.police, "ClearanceRates", update, overwrite = TRUE)
dbDisconnect(cons.police)

####Write to Tableau Dashboard####
write.csv(update, file="U:/CityWide Performance/CovStat/CovStat Projects/Police/Tableau Files/ClearanceRates.csv")

##Write to CovStat Repository####
write.csv(update, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Police/ClearanceRates.csv")


##### Subsetting by Date Test #####
#test <- port.ins[port.ins$NewLeaseDate > as.Date("1899-12-31"),]
#as.Date(x, origin = "1970-01-01")











