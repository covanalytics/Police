
setwd("U:/CityWide Performance/Police/Nuisance Violation")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("magrittr")
library("gmodels")
library("descr")

# Read the contents of the file into a data.frame
nuisance <-  read.xlsx("All_Oct16.xlsx", sheetIndex=1,  header=TRUE, as.data.frame=TRUE)

write.xlsx(nuisance, "O:/AllUsers/CovStat/data Portal/Repository/Data/Police/NuisanceViolations.xlsx")
write.csv(nuisance, "C:/Users/tsink/Mapping/Geocoding/Police/Nuisance Ordiance Violations/NuisanceViolations.csv")

nuisance$Case.Address <- NULL
nuisance$Case.Number <- NULL
nuisance$Case.Significant.Event <- NULL

nuisance <- nuisance[c("Case.Occurred.Incident.Type", "Case.Offense.Date", "Case.Address.Latitude", "Case.Address.Longitude")]
names(nuisance)[1] <- "Case_Offen"
names(nuisance)[2] <- "Case_Repor"

write.csv(nuisance, "C:/Users/tsink/Mapping/Geocoding/Police/Nuisance Ordiance Violations/NuisanceViolations.csv")
#write.xlsx(nuisance, "U:/CityWide Performance/Police/Nuisance Violation/NuisanceViolations.xlsx")


###   SQLite storage ####
police <- dbDriver("SQLite")
policeRuns <- police.runs
policeRuns <- as.data.frame(policeRuns)
cons.police <- dbConnect(police, dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Police.db")
dbWriteTable(cons.police, "PoliceRuns", policeRuns, overwrite = TRUE)
dbDisconnect(cons.police)