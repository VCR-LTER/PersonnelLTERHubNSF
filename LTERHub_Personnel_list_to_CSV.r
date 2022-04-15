# Read site listing from the LTERHub API and save it to a CSV file (omitting some things)
# written by John Porter, 2021

# set the working directory to someplace suitable
setwd("c:/users/john/downloads")
# set the site of interest - must match what is in the database
mySiteName<-'Virginia Coast Reserve LTER'
# set the parameters for the API {site_id} 
mySiteId<-"XXXXXXXXXXXXXXXXXX"
# Set the name of the output file
outFileCSV<-"VCR_Personnel.csv"

library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)


json<-read_file(paste("http://lternet.force.com/services/apexrest/v1/Site/",mySiteId,"/Directory/", sep=''))

list1<-fromJSON(json)
df1<-bind_rows(list1, .id="Person")
df2<-unnest(df1,cols=sites)
df3<-df2[df2$site_name == mySiteName,c("name","email","site_role","ORCID","twitter","linkedin","secondary_email")]
df3$nameLast<-sub('^.* ([[:alnum:]]+)$', '\\1', df3$name)
df3<-df3[order(df3$site_role,df3$nameLast,df3$name),]

write.csv(df3,outFileCSV, row.names=F)
