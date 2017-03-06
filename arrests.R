
setwd("U:/CityWide Performance/CovStat/CovStat Projects/Police/Arrests")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")
library("arcgisbinding")
library("sp")
library("spdep")
library("rgdal")
library("maptools")
library("ggmap")

#Read the contents of all file into a data.frame
####  ARRESTS ####
arrest  <-  read.csv(file="Update.csv", header=TRUE, na.strings = c("", NA), stringsAsFactors = FALSE)
####Clean up date column ####
#arrest$ArrestNumber1 <- substr(arrest$ArrestNumber1, 1, nchar(arrest$ArrestNumber1)-13) # long version for triming from right
arrest$ArrestNumber1 <- str_sub(arrest$ArrestNumber1, -50, -15) # shorter version for triming from right

####Fix Name Columnn ----
arrest$ArresteeName1 <- gsub("DOB:.*", "", arrest$ArresteeName1)
                     
##Coding for DUI and Gun arrests----
arrest$DUI <- ifelse(grepl("OPER MTR", arrest$CrimCharge),"YES", "NO")
arrest$Gun <- ifelse(grepl("HANDGUN", arrest$CrimCharge),"YES", 
              ifelse(grepl("DEADLY WEAPON", arrest$CrimCharge),"YES", "No"))

####Add count column ----
arrest$Count <- 1

####Add zero to NAs in Lat and -360 in Lon ----
for (i in 1:length(arrest$Lat)){
  if(is.na(arrest$Lat[i]) | arrest$Lat[i] <= 0)
    arrest$Lat[i] <- 0
}
for (i in 1:length(arrest$Lon)){
  if(is.na(arrest$Lon[i]) | arrest$Lon[i] == -361)
    arrest$Lon[i] <- 0
}

send_arcgis <- function(dataframe, path, layerName){
  coordinates(dataframe) <- ~Lon+Lat
  ## Define Coordinate system for spatial points data.frame 
  reference <- CRS("+init=epsg:4326")
  proj4string(dataframe) <- reference
  ## Assign closest neighborhood and sector in ArcGIS
  writeOGR(obj = dataframe, dsn = path, layer = layerName, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
}
send_arcgis(arrest, "C:/Users/tsink/Mapping/Geocoding/Police", "ArrestUpdate")


#### Receive from ArcGIS ####
receive_arcgis <- function(fromPath, dataframeName) {
  arc.check_product()
  ## Read GIS Features 
  read <- arc.open(fromPath)
  ## Create Data.Frame from GIS data 
  dataframeName <- arc.select(read)
  ## Bind hidden lat/long coordinates back to data frame 
  shape <- arc.shape(dataframeName)
  dataframeName<- data.frame(dataframeName, long=shape$x, lat=shape$y)
}
arrestGIS <- receive_arcgis("C:/Users/tsink/Mapping/Geocoding/Police/Arrests2.shp", arrestGIS)

#### Mutual Aid ####
# Assign First Mutual Aid ------
arrestGIS$NbhdLabel[arrestGIS$Distance > 0] <- "Mutual Aid"

# Assign Closest Neighborhood for arrests in downtown sector but outside of neighborhod boundary -----
arrestGIS$NbhdLabel <- ifelse(arrestGIS$Distance_1 == 0 & arrestGIS$NbhdLabel == "Mutual Aid", as.character(arrestGIS$NEIGHBORHO),
                            as.character(arrestGIS$NbhdLabel))

# Assign Mutual Aid to police sector -----
arrestGIS$BEAT[arrestGIS$Distance_1 > 0] <- 0

# If call is inside neighborhood but outside sector -----
arrestGIS$NbhdLabel <- ifelse(arrestGIS$Distance == 0 & arrestGIS$Distance_1 > 0, "Mutual Aid",
                            as.character(arrestGIS$NbhdLabel))

##Invalid coordinates and neighborhood indication
arrestGIS$NbhdLabel[arrestGIS$lat == 0] <- "Invalid Coordinates"
arrestGIS$BEAT[arrestGIS$lat == 0] <- 99

#### Delete, Rename, and Order Columns ----
arrestGIS <- arrestGIS[, c(-2:-3, -11:-14, -16:-28, -31:-33)]
names(arrestGIS) <- c("FID", "Date", "Name", "Location", "Crime_Char", "DUI", "Gun", "Count",
                      "NbhdLabel", "ID_1", "Beat", "Long", "Lat")


####  SQLite storage ####
library("RSQLite")
cons.police <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Police.db")
dbWriteTable(cons.police, "Arrests", arrestGIS, append = TRUE)
 
## Tableau Dashboard. Pull the database and refresh ---------------------------
dash_arrest <- dbGetQuery(cons.police, 'select * from Arrests')
dash_arrest$Name <- gsub(", ", "^", dash_arrest$Name)
write.csv(dash_arrest, "U:/CityWide Performance/CovStat/CovStat Projects/Police/Tableau Files/PoliceArrests.csv", row.names = FALSE)
dbDisconnect(cons.police)

### CovStat Repository ---------------------------
write.csv(dash_arrest, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Police/Arrests.csv")

