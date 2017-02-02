
# Data is extracted from Kenton County dispatch report server once a month.
# Data is pulled from CovPD Admin Reports > 24 Hours Shift Report CSV



setwd("U:/CityWide Performance/CovStat/CovStat Projects/Police/Runs/Total Runs")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("magrittr")
library("gmodels")
library("descr")
library("zoo")
library("arcgisbinding")
library("sp")
library("spdep")
library("rgdal")
library("maptools")
library("ggmap")

#Read the contents of the worksheet into a data.frame for the current update
police.runs  <- read.csv("MONTHLY_UPDATE.csv",  header=TRUE,  stringsAsFactors = FALSE)

## Add some data for the update and format dates
police.runs <- within(police.runs, {
  Year <- '2017'
  Count <- 1
  CreateDateTime <- strptime(CreateDateTime, format = "%m/%d/%Y %H:%M")
  
})

##Edit names of columns
names(police.runs) <- c("IncidentNumber", "Date", "Incident.Type", "Address", "Lat", "Long", "Year",  "Count")

police.runs <- police.runs[order(police.runs$Incident.Type),]

#Change new names format to old format
change_names <- function(a = police.runs$Incident.Type){
  a[a == "Abandoned Vehicle-OTP"] <- "Abandoned Vehicle - OTP"
  a[a == "Accident-Hit Skip"] <- "Accident - Hit Skip"
  a[a == "Accident-No Injuries"] <- "Accident - No Injuries"
  a[a == "Accident-w/Injuries"] <- "Accident - w / Injuries"
  a[a == "Alarm-Audible"] <- "Alarm - Audible"
  a[a == "Alarm-Hold Up"] <- "Alarm - Hold Up"
  a[a == "Alarm-Panic"] <- "Alarm - Panic"
  a[a == "Alarm-Intrusion"] <- "Alarm - Intrusion"
  a[a == "Animal-Bite/Attack"] <- "Animal - Bite/Attack"
  a[a == "Animal-Complaint"] <- "Animal - Complaint"
  a[a == "Animal-Vicious"] <- "Animal - Vicious"
  a[a == "Assault-w/injuries"] <- "Assault - w / injuries"
  a[a == "Assist-Fire"] <- "Assist - Fire"
  a[a == "Assist-Other Agency"] <- "Assist - Other Agency"
  a[a == "Assist-Police"] <- "Assist - Police"
  a[a == "Boat-Adrift/Abandoned"] <- "Boat - Adrift / Abandoned"
  a[a == "Dispute-Active"] <- "Dispute - Active"
  a[a == "Dispute-Inactive"] <- "Dispute - Inactive"
  a[a == "Drunk Driver/DUI"] <- "Drunk Driver / DUI"
  a[a == "Fire-Alarm"] <- "Fire - Alarm"
  a[a == "Fire-Investigation"] <- "Fire - Investigation"
  a[a == "Gun-Subject w/"] <- "Gun - Subject w/"
  a[a == "Harassment/Stalking"] <- "Harrassment / Stalking"
  a[a == "Ill / Non-Specific"] <- "Ill / Non-Specific"
  a[a == "Investigation/Follow-Up"] <- "Investigation / Follow-Up"
  a[a == "Loud Music/Noise Complaint"] <- "Loud Music / Noise Complaint"
  a[a == "Loud/Disorderly Subjects"] <- "Loud / Disorderly Subjects"
  a[a == "Missing Person/Runaway"] <- "Missing Person / Runaway"
  a[a == "Other/Unkown (Police)"] <- "Other / Unkown (Police)"
  a[a == "Other/Unknown (Police)"] <- "Other / Unkown (Police)"
  a[a == "Other / Unkown (Police)"] <- "Other / Unkown (Police)"
  a[a == "Overdose / Medication"] <- "Overdose / Medication"
  a[a == "Overdose/Medication"] <- "Overdose / Medication"
  a[a == "Overdose / Medication"] <- "Overdose / Medication"
  a[a == "Property-Lost/Found/Assist"] <- "Property - Lost/Found/Assist"
  a[a == "Public Contact/Complaint"] <- "Public Contact/Complaint"
  a[a == "Public Contact/Complaint"] <- "Public Contact/Complaint"
  a[a == "Rape/Sexual Assault"] <- "Rape / Sexual Assault"
  a[a == "Shooting/Gunshot Wound"] <- "Shooting / Gunshot Wound"
  a[a == "Speeding/Reckless Vehicle"] <- "Speeding / Reckless Vehicle"
  a[a == "Theft-From a Motor Vehicle"] <- "Theft - From a Motor Vehicle"
  a[a == "Theft-Motor Vehicle"] <- "Theft - Motor Vehicle"
  a[a == "Traffic Obstruction"] <- "Traffic Obstruction"
  a[a == "Trouble-Employee/Customer"] <- "Trouble - Employee / Customer"
  a[a == "Trouble-Juvenile"] <- "Trouble - Juvenile"
  a[a == "Trouble-Landlord/Tennant"] <- "Trouble - Landlord / Tennant"
  a[a == "Trouble-Neighbors"] <- "Trouble - Neighbors"
  a[a == "Trouble-Refusing to Leave"] <- "Trouble - Refusing to Leave"
  a[a == "Vacation/Business Check"] <- "Vacation / Business Check"
  a[a == "Warrant-Arrest"] <- "Warrant - Arrest"
  a[a == "Warrant-Search"] <- "Warrant - Search"
  a[a == "Weapon-Subject w/"] <- "Weapon - Subject w/"
  
  return(a)
}
#call function to change new names and check spelling
police.runs$Incident.Type <- change_names()
for(col in 3){
  name_check <- unique(sort(police.runs[,col]))}

#Load the call type keys
KEY_CFS <- read.csv("CFS_KEY_2_1_17.csv", header = TRUE)
KEY_OfficierInitiated <- read.csv("OfficerInitiated_KEY_2_1_17.csv")
KEY_PublicAssistance <- read.csv("PublicAssistance_KEY_2_1_17.csv", header = TRUE)
KEY_SpecialDetail <- read.csv("SpecialDetail_KEY_2_1_17.csv", header = TRUE)
KEY_DNInclude <- read.csv("DNInclude_KEY_2_1_17.csv", header = TRUE)

### Function to assign call types ##
call_type <- function(x = police.runs$Category){
  
  #Create objects and store the data frame vector that contains the call type keys
  CFS <- KEY_CFS$x
  officer <- KEY_OfficierInitiated$x
  public_assistance <- KEY_PublicAssistance$x
  SpDetail <- KEY_SpecialDetail$x
  DNInclude <- KEY_DNInclude$x
  
  #create an object and store a condition that tests if each call type in the police.runs file
  #matches the call type in the key
  match_cfs <- police.runs$Incident.Type %in% CFS
  match_officer <- police.runs$Incident.Type %in% officer
  match_publicAssistance <- police.runs$Incident.Type %in% public_assistance
  match_SpDetail <- police.runs$Incident.Type %in% SpDetail
  match_DINclude <- police.runs$Incident.Type %in% DNInclude
  
  #Create a new column that assigns a category designation to each row where the call type matches the key
  x[match_cfs] <- "Calls for Service"
  x[match_officer] <- "Officier Initiated"
  x[match_publicAssistance] <- "Public Assistance"
  x[match_SpDetail] <- "Special Detail"
  x[match_DINclude] <- "do not include"
  
  return(x)
}
police.runs$Category <- call_type()

###Spell out acronyms for incidents
police.runs$Incident.Type[police.runs$Incident.Type == "TS"] <- "Traffic Stop"
police.runs$Incident.Type <- sub("^ATL$", "Attempt to Locate", police.runs$Incident.Type)
police.runs$Incident.Type <- sub("^SRO$", "School Resource Officer", police.runs$Incident.Type)
police.runs$Incident.Type <- sub("^SO$", "Sheriff", police.runs$Incident.Type)
police.runs$Incident.Type <- sub("^EPO$", "Emergency Protection Order", police.runs$Incident.Type)

###Gun Reported####
police.runs$GunReported <- ifelse(grepl("Gun", police.runs$Incident.Type),"Gun Reported", 
                           ifelse(grepl("Shooting", police.runs$Incident.Type),"Gun Reported", 
                           ifelse(grepl("Shots", police.runs$Incident.Type),"Gun Reported", "No Gun Reported"))) 

#########################################
## do not include administrative calls ##
#########################################
police.runs <- subset(police.runs,  Category != "do not include")

#############################################
####Neighborhood Assignment Current Update###
#############################################
write.csv(police.runs, file="C:/Users/tsink/Mapping/Geocoding/Police/PoliceUpdates.csv", row.names = FALSE)

#####################
##Connect to ArcGIS##
#####################

#### Initialize arcgisbinding ####
arc.check_product()

#### Read GIS Features ####
readGIS<- arc.open("C:/Users/tsink/Mapping/Geocoding/Police/RunsNeighborhoods_Sectors.shp")

#### Create Data.Frame ####
policeGIS <- arc.select(readGIS)

#bind hidden lat/long coordinates back to data frame
#shape <- arc.shape(policeGIS)
#policeGIS<- data.frame(policeGIS, long=shape$x, lat=shape$y)

policeGIS <- policeGIS[, c(-1:-2, -14:-17, -19:-31, -34:-36)] 
names(policeGIS) <- c("Field1", "IncidentNu", "Date", "Incident Type", "Address", "Lat", "Long",
                      "Year", "Count", "Category", "GunReported", "NbhdLabel", "ID_1", "BEAT")

#########################
####  SQLite storage ####
#########################
library("RSQLite")
cons.police <- dbConnect(drv=RSQLite::SQLite(), dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Police.db")
dbWriteTable(cons.police, "PoliceRuns", policeGIS, overwrite = TRUE)
dbDisconnect(cons.police)

###CovStat Repository####
write.csv(police.runs, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Police/Police Runs.csv")



## Load from SQLite -----------------
alltables <- dbListTables(cons.police)
police_history <- dbGetQuery(cons.police, 'select * from PoliceRuns')
police_history <- strptime(police_history$Date, format = "%m/%d/%Y %I:%M %p")

#########################################################
##Reload and output for OpenGov Map showing repat calls##
#########################################################
police.map <- police.runs
#police.map$Date <- gsub(" \\d*[[:punct:]]*\\d*", "", police.map$Date)
##Split Date and Time into separate columns
police.map <- cSplit(police.map, "Date", sep = " ", type.convert = character)
names(police.map)[13] <- "Date"
names(police.map)[14] <- "Time"
police.map$Date_3 <- NULL

#police.map$Date <- gsub(" \\d*\\d*\\s[[:upper:]]*", "", police.map$Date)
#police.map$Incident.Group[which(is.na(police.map$Incident.Group))] <- "Special Detail"
#police.map$Field1 <- NULL
police.map <- subset(police.map, Update.Month == "16-Dec" & Lat >= 0 & Category == "Calls for Service")

police.map$MultipleRuns[duplicated(police.map$Lat) | duplicated(police.map$Lat, fromLast = TRUE) 
                        & duplicated(police.map$Long) | duplicated(police.map$Long, fromLast = TRUE)] <- "Repeat Calls"

police.map$MultipleRuns[which(is.na(police.map$MultipleRuns))] <- "No Multiple Runs"

write.csv(police.map, "U:/OpenGov/Unique Reports/Police/Total Runs/OpenGov Map/LoadDec16.csv", row.names = FALSE)



##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


###Assign to "Crimes Against Persons"
police.runs <- within(police.runs, {
  Incident.Group <- NA
  Incident.Group [Incident.Type == "Assault"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Assault - w / injuries"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Child Abuse"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Harrassment / Stalking"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Rape / Sexual Assault"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Robbery"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Sex Offense"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Shooting / Gunshot Wound"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Stabbing"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Prowler"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Bomb Threat"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Custodial Interference"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Indecent Exposure"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Forgery"] <- "Crimes Against Persons"
  Incident.Group [Incident.Type == "Kidnapping/False Imprisonment"] <- "Crimes Against Persons"
  
  Incident.Group [Incident.Type == "Drug Activity"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Drug Investigation"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Prostitution"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Intoxicated Person"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Panhandling"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Shots Fired"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Gambling"] <- "Drugs and Vice"
  Incident.Group [Incident.Type == "Overdose / Medication"] <- "Drugs and Vice"
  
  Incident.Group [Incident.Type == "Criminal Mischief"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Burglary"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Arson"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Damage to Property"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Fraud"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Shoplifting"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Theft - Motor Vehicle"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Theft - From a Motor Vehicle"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Theft"] <- "Property Crime/Theft"
  Incident.Group [Incident.Type == "Trespass"] <- "Property Crime/Theft"
  
  Incident.Group [Incident.Type == "Drunk Driver / DUI"] <- "Traffic"
  Incident.Group [Incident.Type == "Intoxicated Driver"] <- "Traffic"
  Incident.Group [Incident.Type == "Accident - w / Injuries"] <- "Traffic"
  Incident.Group [Incident.Type == "Accident - Hit Skip"] <- "Traffic"
  Incident.Group [Incident.Type == "Accident - No Injuries"] <- "Traffic"
  Incident.Group [Incident.Type == "Accident - Boat"] <- "Traffic"
  Incident.Group [Incident.Type == "Accident - Train Wreck"] <- "Traffic"
  Incident.Group [Incident.Type == "Boat - Adrift / Abandoned"] <- "Traffic"
  Incident.Group [Incident.Type == "Traffic Complaint/ Investigation"] <- "Traffic"
  Incident.Group [Incident.Type == "Speeding / Reckless Vehicle"] <- "Traffic"
  Incident.Group [Incident.Type == "Airplane Crash"] <- "Traffic"
  Incident.Group [Incident.Type == "Traffic Stop"] <- "Traffic"
  Incident.Group [Incident.Type == "Traffic Obstruction"] <- "Traffic"
  
  Incident.Group [Incident.Type == "Dispute - Active"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Dispute - Inactive"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Alarm - Audible"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Alarm - Hold Up"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Alarm - Intrusion"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Alarm - Panic"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "All Other Offense"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Animal AC Call"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Animal - Bite/Attack"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Animal - Complaint"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Animal - Vicious"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Assist - Police"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Bar Check"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Death Investigation"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Disorderly Conduct"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Domestic Trouble"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Drowning"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Fight"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Fire - Alarm"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Fire - Investigation"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Fireworks Complaint"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Follow Up"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Gun - Subject w/"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Investigation / Follow-Up"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Juvenile"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Loud / Disorderly Subjects"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Loud Music / Noise Complaint"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Missing Person / Runaway"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Open Door/Window"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Ordinance Violation"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Public Contact / Complaint"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Routine Investigation"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Special Area Check"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Special Investigation"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Suspicious Activity"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Suspicious Person"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Suspicious Person/Vehicle"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Trouble"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Trouble - Employee / Customer"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Trouble - Juvenile"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Trouble - Landlord / Tennant"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Trouble - Neighbors"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Trouble - Refusing to Leave"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Warrant - Arrest"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Warrant - Search"] <- "Miscellaneous"
  Incident.Group [Incident.Type == "Weapon - Subject w/"] <- "Miscellaneous"
  
  Incident.Group [Incident.Type == "911 Disconnect"] <- "Other"
  Incident.Group [Incident.Type == "911 Open Line"] <- "Other"
  Incident.Group [Incident.Type == "911 Open Line Trouble"] <- "Other"
  Incident.Group [Incident.Type == "Foot Patrol"] <- "Other"
  Incident.Group [Incident.Type == "Officer Initiated"] <- "Other"
  Incident.Group [Incident.Type == "Other / Unknown (Police)"] <- "Other"
  Incident.Group [Incident.Type == "Police Services"] <- "Other"
  Incident.Group [Incident.Type == "Pursuit"] <- "Other"
  Incident.Group [Incident.Type == "SWAT Callout"] <- "Other"
  Incident.Group [Incident.Type == "Ill / Non-Specific"] <- "Other"})                                     
                                                                          
###Assign to "Assisting the Public"
police.runs$Incident.Group[police.runs$Category=="Public Assistance"] <- "Assisting the Public"




  





 
                                    
         

                   
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                    
