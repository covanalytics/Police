
setwd("U:/OpenGov/Unique Reports/Police/Total Runs")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("splitstackshape")
library("magrittr")
library("gmodels")
library("descr")
library("RSQLite")

#Read the contents of the worksheet into a data.frame
police.runs  <- read.csv("RUNS.csv", header=TRUE,  stringsAsFactors = FALSE)
police.runs$Count <- 1
##Split Date and Time into separate columns
#police.runs <- cSplit(police.runs, "CreateDateTime", sep = " ", type.convert = character)

#police.runs$CreateDateTime_3 <- NULL

##Edit names of columns
names(police.runs) <- c("IncidentNumber", "Date", "Incident.Type", "Address", "Lat", "Long", "Year", "FiscalYear", "Update.Month", "Count")

police.runs <- police.runs[order(police.runs$Incident.Type),]



change_names <- function(a = police.runs$Incident.Type){
  a[a == "Abandoned Vehicle-OTP"] <- "Abandoned Vehicle - OTP"
  a[a == "Accident-Hit Skip"] <- "Accident - Hit Skip"
  a[a == "Accident-No Injuries"] <- "Accident - No Injuries"
  a[a == "Accident-w/Injuries"] <- "Accident - w / Injuries"
  a[a == "Alarm-Audible"] <- "Alarm - Audible"
  a[a == "Alarm-Hold Up"] <- "Alarm - Hold Up"
  a[a == "Alarm-Panic"] <- "Alarm - Panic"
  a[a == "Animal-Bite/Attack"] <- "Animal - Bite/Attack"
  a[a == "Animal-Complaint"] <- "Animal - Complaint"
  a[a == "Animal-Vicious"] <- "Animal - Vicious"
  a[a == "Assault-w/injuries"] <- "Assault - w / injuries"
  a[a == "Assist-Fire"] <- "Assist - Fire"
  a[a == "Assist-Other Agency"] <- "Assist - Other Agency"
  a[a == "Dispute-Active"] <- "Dispute - Active"
  a[a == "Dispute-Inactive"] <- "Dispute - Inactive"
  a[a == "Drunk Driver/DUI"] <- "Drunk Driver / DUI"
  a[a == "Gun-Subject w/"] <- "Gun - Subject w/"
  a[a == "Harassment/Stalking"] <- "Harrassment / Stalking"
  a[a == "Ill / Non-Specific"] <- "Ill / Non-Specific"
  a[a == "Investigation/Follow-Up"] <- "Investigation / Follow-Up"
  a[a == "Loud Music/Noise Complaint"] <- "Loud Music / Noise Complaint"
  a[a == "Loud/Disorderly Subjects"] <- "Loud / Disorderly Subjects"
  a[a == "Missing Person/Runaway"] <- "Missing Person / Runaway"
  a[a == "Other/Unkown (Police)"] <- "Other / Unkown (Police)"
  a[a == "Overdose / Medication"] <- "Overdose / Medication"
  a[a == "Overdose/Medication"] <- "Overdose / Medication"
  a[a == "Property-Lost/Found/Assist"] <- "Property - Lost/Found/Assist"
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
name_check <- for(col in 3)
  print(unique(sort(police.runs[,col])))

###Police Knowledge Codes
####Calls for Service####
police.runs <- within(police.runs, {
  Category <- NA
  Category [Incident.Type == "911 Disconnect"] <- "Calls for Service"
  Category [Incident.Type == "911 Open Line"] <- "Calls for Service"
  Category [Incident.Type == "911 Open Line Trouble"] <- "Calls for Service"
  Category [Incident.Type == "Accident - Boat"] <- "Calls for Service"
  Category [Incident.Type == "Accident - Hit Skip"] <- "Calls for Service"
  Category [Incident.Type == "Accident - No Injuries"] <- "Calls for Service"
  Category [Incident.Type == "Accident-No Injuries"] <- "Calls for Service"
  Category [Incident.Type == "Accident - Train Wreck"] <- "Calls for Service"
  Category [Incident.Type == "Accident - w / Injuries"] <- "Calls for Service"
  Category [Incident.Type == "Airplane Crash"] <- "Calls for Service"
  Category [Incident.Type == "Alarm - Audible"] <- "Calls for Service"
  Category [Incident.Type == "Alarm - Hold Up"] <- "Calls for Service"
  Category [Incident.Type == "Alarm - Intrusion"] <- "Calls for Service"
  Category [Incident.Type == "Alarm - Panic"] <- "Calls for Service"
  Category [Incident.Type == "All Other Offense"] <- "Calls for Service"
  Category [Incident.Type == "Animal - Bite/Attack"] <- "Calls for Service"
  Category [Incident.Type == "Animal - Complaint"] <- "Calls for Service"
  Category [Incident.Type == "Animal - Vicious"] <- "Calls for Service"
  Category [Incident.Type == "Animal AC Call"] <- "Calls for Service"
  Category [Incident.Type == "Arson"] <- "Calls for Service"
  Category [Incident.Type == "Assault"] <- "Calls for Service"
  Category [Incident.Type == "Assault - w / injuries"] <- "Calls for Service"
  Category [Incident.Type == "Assist - Police"] <- "Calls for Service"
  Category [Incident.Type == "Boat - Adrift / Abandoned"] <- "Calls for Service"
  Category [Incident.Type == "Bomb Threat"] <- "Calls for Service"
  Category [Incident.Type == "Burglary"] <- "Calls for Service"
  Category [Incident.Type == "Child Abuse"] <- "Calls for Service"
  Category [Incident.Type == "Criminal Mischief"] <- "Calls for Service"
  Category [Incident.Type == "Custodial Interference"] <- "Calls for Service"
  Category [Incident.Type == "Damage to Property"] <- "Calls for Service"
  Category [Incident.Type == "Death Investigation"] <- "Calls for Service"
  Category [Incident.Type == "Disorderly Conduct"] <- "Calls for Service"
  Category [Incident.Type == "Dispute - Active"] <- "Calls for Service"
  Category [Incident.Type == "Dispute - Inactive"] <- "Calls for Service"
  Category [Incident.Type == "Domestic Trouble"] <- "Calls for Service"
  Category [Incident.Type == "Drowning"] <- "Calls for Service"
  Category [Incident.Type == "Drug Activity"] <- "Calls for Service"
  Category [Incident.Type == "Drunk Driver / DUI"] <- "Calls for Service"
  Category [Incident.Type == "Intoxicated Driver"] <- "Calls for Service"
  Category [Incident.Type == "Fight"] <- "Calls for Service"
  Category [Incident.Type == "Fire - Alarm"] <- "Calls for Service"
  Category [Incident.Type == "Fire - Investigation"] <- "Calls for Service"
  Category [Incident.Type == "Fireworks Complaint"] <- "Calls for Service"
  Category [Incident.Type == "Forgery"] <- "Calls for Service"
  Category [Incident.Type == "Fraud"] <- "Calls for Service"
  Category [Incident.Type == "Gambling"] <- "Calls for Service"
  Category [Incident.Type == "Gun - Subject w/"] <- "Calls for Service"
  Category [Incident.Type == "Harrassment / Stalking"] <- "Calls for Service"
  Category [Incident.Type == "Indecent Exposure"] <- "Calls for Service"
  Category [Incident.Type == "Intoxicated Person"] <- "Calls for Service"
  Category [Incident.Type == "Juvenile"] <- "Calls for Service"
  Category [Incident.Type == "Kidnapping/False Imprisonment"] <- "Calls for Service"
  Category [Incident.Type == "Loud / Disorderly Subjects"] <- "Calls for Service"
  Category [Incident.Type == "Loud Music / Noise Complaint"] <- "Calls for Service"
  Category [Incident.Type == "Missing Person / Runaway"] <- "Calls for Service"
  Category [Incident.Type == "New Call"] <- "Calls for Service"
  Category [Incident.Type == "Open Door/Window"] <- "Calls for Service"
  Category [Incident.Type == "Ordinance Violation"] <- "Calls for Service"
  Category [Incident.Type == "Other / Unknown (Police)"] <- "Calls for Service"
  Category [Incident.Type == "Panhandling"] <- "Calls for Service"
  Category [Incident.Type == "Prostitution"] <- "Calls for Service"
  Category [Incident.Type == "Prowler"] <- "Calls for Service"
  Category [Incident.Type == "Rape / Sexual Assault"] <- "Calls for Service"
  Category [Incident.Type == "Robbery"] <- "Calls for Service"
  Category [Incident.Type == "Sex Offense"] <- "Calls for Service"
  Category [Incident.Type == "Shooting / Gunshot Wound"] <- "Calls for Service"
  Category [Incident.Type == "Shoplifting"] <- "Calls for Service"
  Category [Incident.Type == "Shots Fired"] <- "Calls for Service"
  Category [Incident.Type == "Speeding / Reckless Vehicle"] <- "Calls for Service"
  Category [Incident.Type == "Stabbing"] <- "Calls for Service"
  Category [Incident.Type == "Suspicious Activity"] <- "Calls for Service"
  Category [Incident.Type == "Suspicious Person"] <- "Calls for Service"
  Category [Incident.Type == "Suspicious Person/Vehicle"] <- "Calls for Service"
  Category [Incident.Type == "Theft"] <- "Calls for Service"
  Category [Incident.Type == "Theft - From a Motor Vehicle"] <- "Calls for Service"
  Category [Incident.Type == "Theft - Motor Vehicle"] <- "Calls for Service"
  Category [Incident.Type == "Traffic Complaint/ Investigation"] <- "Calls for Service"
  Category [Incident.Type == "Trespass"] <- "Calls for Service"
  Category [Incident.Type == "Trouble"] <- "Calls for Service"
  Category [Incident.Type == "Trouble - Employee / Customer"] <- "Calls for Service"
  Category [Incident.Type == "Trouble - Juvenile"] <- "Calls for Service"
  Category [Incident.Type == "Trouble - Landlord / Tennant"] <- "Calls for Service"
  Category [Incident.Type == "Trouble - Neighbors"] <- "Calls for Service"
  Category [Incident.Type == "Trouble - Refusing to Leave"] <- "Calls for Service"
  Category [Incident.Type == "Warrant - Arrest"] <- "Calls for Service"
  Category [Incident.Type == "Weapon - Subject w/"] <- "Calls for Service"})

####do not include####
police.runs <- within(police.runs,{
  Category [Incident.Type == "Administrative"] <- "do not include"
  Category [Incident.Type == "AFIS"] <- "do not include"
  Category [Incident.Type == "Imported Report"] <- "do not include"
  Category [Incident.Type == "Mobil Forensice"] <- "do not include"
  Category [Incident.Type == "Training"] <- "do not include"
  Category [Incident.Type == "TX"] <- "do not include"
  Category [Incident.Type == "TXAC"] <- "do not include"
  Category [Incident.Type == "TXSO"] <- "do not include"
  Category [Incident.Type == "Vehicle Service"] <- "do not include"
  Category [Incident.Type == "New Call"] <- "do not include"
  Category [Incident.Type == "Broken/Fractured Bone"] <- "do not include"
  Category [Incident.Type == "EPO Service"] <- "do not include"
  Category [Incident.Type == "SRO/Detail/Home Visit"] <- "do not include"
  Category [Incident.Type == "Eviction/Court Order"] <- "do not include"
  Category [Incident.Type == "SO Transport"] <- "do not include"
  Category [Incident.Type == "Fire - Alarm"] <- "do not include"
  Category [Incident.Type == "Water"] <- "do not include"
  Category [Incident.Type == "Ill/Non-Specific"] <- "do not include"})

###Officer Initiated####
police.runs <- within(police.runs,{
  Category [Incident.Type == "Bar Check"] <- "Officier Initiated"
  Category [Incident.Type == "Drug Investigation"] <- "Officier Initiated"
  Category [Incident.Type == "Follow Up"] <- "Officier Initiated"
  Category [Incident.Type == "Foot Patrol"] <- "Officier Initiated"
  Category [Incident.Type == "Investigation / Follow-Up"] <- "Officier Initiated"
  Category [Incident.Type == "Officer Initiated"] <- "Officier Initiated"
  Category [Incident.Type == "Police Services"] <- "Officier Initiated"
  Category [Incident.Type == "Public Contact / Complaint"] <- "Officier Initiated"
  Category [Incident.Type == "Pursuit"] <- "Officier Initiated"
  Category [Incident.Type == "Routine Investigation"] <- "Officier Initiated"
  Category [Incident.Type == "Special Area Check"] <- "Officier Initiated"
  Category [Incident.Type == "Special Investigation"] <- "Officier Initiated"
  Category [Incident.Type == "SWAT Callout"] <- "Officier Initiated"
  Category [Incident.Type == "Traffic Stop"] <- "Officier Initiated"
  Category [Incident.Type == "TS"] <- "Officier Initiated"
  Category [Incident.Type == "Warrant - Search"] <- "Officier Initiated"})

###Public Assistance####
police.runs <- within(police.runs, {
  Category [Incident.Type == "Abandoned Vehicle - OTP"] <- "Public Assistance"
  Category [Incident.Type == "Abandoned Vehicle-OTP"] <- "Public Assistance"
  Category [Incident.Type == "Assist - Fire"] <- "Public Assistance"
  Category [Incident.Type == "Assist - Other Agency"] <- "Public Assistance"
  Category [Incident.Type == "ATL"] <- "Public Assistance"
  Category [Incident.Type == "Broken / Fractured Bone"] <- "Public Assistance"
  Category [Incident.Type == "Child Found"] <- "Public Assistance"
  Category [Incident.Type == "Crossing Guard"] <- "Public Assistance"
  Category [Incident.Type == "Emotional Crisis"] <- "Public Assistance"
  Category [Incident.Type == "EPO Service"] <- "Public Assistance"
  Category [Incident.Type == "Eviction / Court Order"] <- "Public Assistance"
  Category [Incident.Type == "Eviction Service"] <- "Public Assistance"
  Category [Incident.Type == "Fire - Structure Fire"] <- "Public Assistance"
  Category [Incident.Type == "General Relay"] <- "Public Assistance"
  Category [Incident.Type == "Incident Report"] <- "Public Assistance"
  Category [Incident.Type == "Lockout Veh/Res"] <- "Public Assistance"
  Category [Incident.Type == "Medical Call"] <- "Public Assistance"
  Category [Incident.Type == "Mentally Ill"] <- "Public Assistance"
  Category [Incident.Type == "Motorist Assist"] <- "Public Assistance"
  Category [Incident.Type == "Notification"] <- "Public Assistance"
  Category [Incident.Type == "Overdose / Drug"] <- "Public Assistance"
  Category [Incident.Type == "Paper Process"] <- "Public Assistance"
  Category [Incident.Type == "Parking Complaint"] <- "Public Assistance"
  Category [Incident.Type == "Property - Lost/Found/Assist"] <- "Public Assistance"
  Category [Incident.Type == "Public Assist"] <- "Public Assistance"
  Category [Incident.Type == "Repo/PP Tow"] <- "Public Assistance"
  Category [Incident.Type == "See Complainant"] <- "Public Assistance"
  Category [Incident.Type == "Sick/Injured Person"] <- "Public Assistance"
  Category [Incident.Type == "SO Transport"] <- "Public Assistance"
  Category [Incident.Type == "SRO / Detail / Home Visit"] <- "Public Assistance"
  Category [Incident.Type == "Traffic Control"] <- "Public Assistance"
  Category [Incident.Type == "Transport"] <- "Public Assistance"
  Category [Incident.Type == "Vacation / Business Check"] <- "Public Assistance"
  Category [Incident.Type == "Vehicle Fire"] <- "Public Assistance"
  Category [Incident.Type == "Well Being Check"] <- "Public Assistance"
  Category [Incident.Type == "Wires Down"] <- "Public Assistance"})


###Spell out acronyms
police.runs$Incident.Type[police.runs$Incident.Type == "TS"] <- "Traffic Stop"
police.runs$Incident.Type <- sub("^ATL$", "Attempt to Locate", police.runs$Incident.Type)
police.runs$Incident.Type <- sub("^SRO$", "School Resource Officer", police.runs$Incident.Type)
police.runs$Incident.Type <- sub("^SO$", "Sheriff", police.runs$Incident.Type)
police.runs$Incident.Type <- sub("^EPO$", "Emergency Protection Order", police.runs$Incident.Type)

###Gun Reported####
police.runs$GunReported <- ifelse(grepl("Gun", police.runs$Incident.Type),"Gun Reported", 
                           ifelse(grepl("Shooting", police.runs$Incident.Type),"Gun Reported", 
                           ifelse(grepl("Shots", police.runs$Incident.Type),"Gun Reported", "No Gun Reported"))) 

##Special Detail
police.runs$Category[police.runs$Incident.Type == "Special Detail"] <- "Special Detail"

#police.runs <- police.runs$Address[!grepl("POLICE MEMORIAL DR", police.runs$Address)]

police.runs <- police.runs %>%
    subset(Address != "1 POLICE MEMORIAL")%>%
    subset(Address != "1 POLICE MEMORIAL DR")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt.1")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. 1")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. DR")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. FL 1")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. GR")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. p")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. PUBL")%>%
    subset(Address != "1 POLICE MEMORIAL DR - Apt. s")%>%
    subset(Address != "POLICE MEMORIAL DR")%>%
    subset(Category != "do not include")

police.runs$Station <- ifelse(grepl("POLICE", police.runs$Address), "Police Station", "Not Police Station")

police.runs <- subset(police.runs, Station != "Police Station")
police.runs$Station <- NULL


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


###CovStat Repository####
write.csv(police.runs, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Police/Police Runs.csv")

###   SQLite storage ####
police <- dbDriver("SQLite")
policeRuns <- police.runs
policeRuns <- as.data.frame(policeRuns)
cons.police <- dbConnect(police, dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/Police.db")
dbWriteTable(cons.police, "PoliceRuns", policeRuns, overwrite = TRUE)
dbDisconnect(cons.police)

########################################
### For Calls for Service Animation  ###
police.runsA <- police.runs
police.runsA <- subset(police.runsA, Update.Month == "16-Nov" & Category == "Calls for Service")
write.csv(police.runsA, file="U:/OpenGov/Unique Reports/Police/Total Runs/Animations/Police RunsNov16.csv")

########################################
### Crimes Against Persons Animation  ###
police.runsA <- police.runs
police.runsA <- subset(police.runsA, Update.Month == "16-Nov" & Incident.Group == "Crimes Against Persons")
write.csv(police.runsA, file="U:/OpenGov/Unique Reports/Police/Total Runs/Animations/CrimesAgainstPersonsNov16.csv")

########################################
### Drugs and Vice Animation  ###
police.runsA <- police.runs
police.runsA <- subset(police.runsA, Update.Month == "16-Nov" & Incident.Group == "Drugs and Vice")
write.csv(police.runsA, file="U:/OpenGov/Unique Reports/Police/Total Runs/Animations/DrugsandViceNov16.csv")

########################################
### Traffic Animation  ###
police.runsA <- police.runs
police.runsA <- subset(police.runsA, Update.Month == "16-Nov" & Incident.Group == "Traffic")
write.csv(police.runsA, file="U:/OpenGov/Unique Reports/Police/Total Runs/Animations/TrafficNov16.csv")



###################################
####For Geocoding Current Update###
###################################
##Don't forget to change the date field to date in EXCEL before loading in ArcGIS.  Otherwise, AM/PM is cut off.
police.runsNov <- police.runs
police.runsNov <- subset(police.runsNov, Update.Month == "16-Nov" | Update.Month == "16-Oct")
write.csv(police.runsNov, file="C:/Users/tsink/Mapping/Geocoding/Police/PoliceRunsNovember2016.csv")


############################ Open Gov############
names(police.runs)[2] <- "Incident Type"
police.runs <- subset(police.runs, `Incident Type` != "Special Detail" & Category != "do not include" & FiscalYear != 2013)
#police.runs <- subset(police.runs, Category != "do not include")
write.csv(police.runs, file="O:/AllUsers/CovStat/Data Portal/Repository/Data/Police/Police Runs.csv")


#######################################################
###  For OpenGov  ###
###OpenGov Data####
pcalls <- police.runs
pcalls <- aggregate(Count ~ FiscalYear + `Incident Type` + Incident.Group + Category, police.runs, sum)
pcalls <- pcalls[order(pcalls$FiscalYear, -pcalls$Count),]
names(pcalls) <- c("Fiscal Year", "Incident Type", "Incident Group", "Police Action", "Count")

#pcalls <- subset(pcalls, `Fiscal Year` != "2017" )

write.csv(pcalls, file="U:/OpenGov/Unique Reports/Police/Total Runs/PoliceRunsOpenGov10.14.16.csv", row.names = FALSE)



        
                                    
         

                   
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                       
                                                                                                    
