rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

library(lubridate)
library(ggplot2)
library(readr)

# Find Files and project size ---------------------------------------------

a<-data.frame(Path=list.files(path = "D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis",full.names = T,recursive = T))
a<-filter(a,str_detect(a$Path,"Log_Files"),str_detect(a$Path,".csv"),!str_detect(a$Path,".jpg")) %>% mutate(Path=as.character(Path))
a$Project<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
a$Project<-str_extract(a$Project,"[^/]*$")

a$year<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
a$year<-str_extract(a$year,"[^/]*$")

# this is a list of all the species that we use, we will then loop through and
# extract each speices for each log file
Sp<-read.csv("D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/Timesheets/ProjectEffiecency/Species_extract.csv",stringsAsFactors = F)
a$Species<-NA
for(i in 1:nrow(Sp)){
  a$Species[str_detect(string = basename(a$Path),Sp$Species[i])]<-Sp$Species[i]
} 
a$filename<-(basename(a$Path))
a$Species[str_detect(string = basename(a$Path),"UMP")]<-"Strike"
a$Species<-toupper(a$Species)

# filter for only 2014-2016 and drop problem child
a<-filter(a,year%in%2014:2017,Project!="FWS_Rose_Atoll_2014",Species=="HAPE") 

# count unique species for each project and list the spp. names 
Spp<-a %>% group_by(Project) %>% 
  summarise(NumSpp=length(unique(Species[!is.na(Species)])),
            SpeciesNames=paste(list(unique(Species[!is.na(Species)]))))

# this goes through all the log files and reads the CSV in to R
data.list1 <- lapply(a$Path, fread,sep=",")
head(data.list1,1)

# Add a project 

for(i in 1:length(a$Project)){
  
  data.list1[[i]]$Project<-a$Project[i]
  data.list1[[i]]$location<-as.character(data.list1[[i]]$location)
  data.list1[[i]]$csv_path<-a$Path[i]
  
}
Names<-NULL
for(i in 1:length(a$Project)){
  Names<-c(Names,names(data.list1[[i]]))
}
# make all the "locations" columns character
for(i in c(1:length(data.list1))){
  
  data.list1[[i]]$location<-as.character(data.list1[[i]]$location)
}
# add file_duration_sec if missing
for(i in c(1:length(data.list1))){
  if(!"file_duration_sec" %in% colnames(data.list1[[i]])) next
  data.list1[[i]]<-select(data.list1[[i]],Project,location,file,file_duration_sec)
}

# concatenate into one big data.frame and keep only the columns that I want 
OurData <- bind_rows(data.list1) %>% 
  select(Project,location,file,file_duration_sec,year,month,day,hour,minute,second,csv_path,start_in_file,end_in_file,rating) 

OurData$csv_file<- tools::file_path_sans_ext(basename(OurData$csv_path))

table(OurData$rating)
