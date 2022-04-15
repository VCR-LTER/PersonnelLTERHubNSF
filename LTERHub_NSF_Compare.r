# Read site listing from the LTERHub API and save it to a CSV file (omitting some things)
# written by John Porter, 2021
rm (list=ls())
# set the working directory to someplace suitable
setwd("c:/users/john/downloads")
# set the site of interest - must match what is in the database
mySiteName<-'Virginia Coast Reserve LTER'
# Site Abbreviation (3 letters)
mySiteAbbreviation<-'VCR'
# set the parameters for the API {site_id} 
# see https://docs.google.com/spreadsheets/d/1gZFux8xjFO4NEfjneGi3R86eKqU2zBvuBk3nUOQWJzU/edit#gid=849272852
mySiteId<-"XXXXXXXXXXXXXXX"
# Set the name of the output file
outFileCSV<-"VCR_Personnel.csv"

library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(xlsx)


json<-read_file(paste("http://lternet.force.com/services/apexrest/v1/Site/",mySiteId,"/Directory/", sep=''))

list1<-fromJSON(json)
df1<-bind_rows(list1, .id="Person")
df2<-unnest(df1,cols=sites)
df3<-df2[df2$site_name == mySiteName,c("name","email","site_role","ORCID","twitter","linkedin","secondary_email","contact_id")]
# parse names from overall name string
df3$nameFirst<-trimws(sub(' .+$', "", df3$name))
df3$nameLast<-sub('^.* ', "", df3$name)
df3$nameMiddle<-trimws(substr(df3$name,(nchar(df3$nameFirst)+1),(nchar(df3$name)-nchar(df3$nameLast))))

df3<-df3[order(df3$site_role,df3$nameLast,df3$name),]

# Read NSF sheet
df4<- read.xlsx("participantsFile-Final2021.xlsx",1,startRow=7)
df5<- merge(df3,df4,by.x=c("nameLast","nameFirst"),by.y=c("X.Last.Name","X.First.Name"),all=T)
nsfOnlyDf<-df5[is.na(df5$email),]
nsfOnlyDf<-nsfOnlyDf[!is.na(nsfOnlyDf$nameLast),]

emailChangedDf<-df5[df5$X.E.mail.Address != df5$email & !is.na(df5$email) & !is.na(df5$X.E.mail.Address),]
lterRoleList<-c("Lead Principal Investigator","Co-Principal Investigator","Investigator",
                "Postdoctoral Associate","Graduate Student","Undergraduate Student","Information Manager",
                "Education Manager","Other Professional","Technical/Research Staff","Administrative Staff",
                "Other Staff","Interested Party","Retired","K-12 Teacher","Volunteer","Media")

nsfOnlyDf$Site<-mySiteAbbreviation
nsfOnlyDf$First_Name<-nsfOnlyDf$nameFirst
nsfOnlyDf$Middle_Name<-nsfOnlyDf$Middle.Name
nsfOnlyDf$Last_Name<-nsfOnlyDf$nameLast
nsfOnlyDf$email_address<-nsfOnlyDf$X.E.mail.Address
nsfOnlyDf$Status<-"Current"
nsfOnlyDf$Site_Role_1<-nsfOnlyDf$X.Most.Senior.Project.Role
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1=="Co-Investigator","Investigator",nsfOnlyDf$Site_Role_1)
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1=="Graduate Student (research assistant)","Graduate Student",nsfOnlyDf$Site_Role_1)
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1=="Postdoctoral (scholar, fellow or other postdoctoral position)","Postdoctoral Associate",nsfOnlyDf$Site_Role_1)
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1=="Co-Investigator","Investigator",nsfOnlyDf$Site_Role_1)
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1=="Technician","Technical/Research Staff",nsfOnlyDf$Site_Role_1)
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1=="Research Experience for Undergraduates (REU) Participant","Undergraduate Student",nsfOnlyDf$Site_Role_1)
nsfOnlyDf$Site_Role_1<-ifelse(nsfOnlyDf$Site_Role_1 %in% lterRoleList,nsfOnlyDf$Site_Role_1,"Interested Party")
nsfOnlyDf$Role_1_Start<-format(Sys.Date(),"%Y")
nsfOnlyDf$Role_1_end<-""
nsfOnlyDf$Site_Role_2<-""
nsfOnlyDf$Role_2_start<-""
nsfOnlyDf$Role_2_end<-""
nsfOnlyDf$contact_id<-""

lterAddDf<-nsfOnlyDf[,c("Site","First_Name","Middle_Name","Last_Name","email_address","Status","Site_Role_1","Role_1_Start","Role_1_end","Site_Role_2","Role_2_start","Role_2_end","contact_id")]

# Now figure out who should be set to Former in the LTER Directory
lterOnlyDf<-df5[is.na(df5$X.E.mail.Address),]
# Assume that if site_role is NA that they are already removed
lterOnlyDf<-lterOnlyDf[!is.na(lterOnlyDf$site_role),]
lterOnlyDf$Site<-mySiteAbbreviation
lterOnlyDf$Status<-"Former"
lterRemoveDf<-lterOnlyDf[!is.na(lterOnlyDf$nameLast),c("Site","nameFirst","nameMiddle","nameLast","email","Status","site_role","contact_id")]
colnames(lterRemoveDf)<-c("Site","First_Name","Middle_Name","Last_Name","email_address","Status","Site_Role_1","contact_id")
lterRemoveDf$Role_1_Start<-""
lterRemoveDf$Role_1_end<-format(Sys.Date(),"%Y")
lterRemoveDf$Site_Role_2<-""
lterRemoveDf$Role_2_start<-""
lterRemoveDf$Role_2_end<-""
lterRemoveDf<-lterRemoveDf[,c("Site","First_Name","Middle_Name","Last_Name","email_address","Status","Site_Role_1","Role_1_Start","Role_1_end","Site_Role_2","Role_2_start","Role_2_end","contact_id")]

lterChangesDf<-rbind(lterAddDf,lterRemoveDf)
lterChangesDf<-lterChangesDf[order(lterChangesDf$Last_Name,lterChangesDf$First_Name,lterChangesDf$Status),]
write.csv(lterChangesDf,outFileCSV,row.names=F)
