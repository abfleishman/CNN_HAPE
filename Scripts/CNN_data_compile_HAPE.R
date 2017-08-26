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
a<-filter(a,year%in%2014:2017,Project!="FWS_Rose_Atoll_2014",Species=="HAPE",!grepl("ToAudit|NoAudit|Eval|Comparison",Path)) 

# count unique species for each project and list the spp. names 
Spp<-a %>% group_by(Project) %>% 
  summarise(NumSpp=length(unique(Species[!is.na(Species)])),
            SpeciesNames=paste(list(unique(Species[!is.na(Species)]))))

# this goes through all the log files and reads the CSV in to R
data.list1 <- lapply(a$Path, fread,sep=",")
head(data.list1,1)

# Add a project, fix location and rating, and add the csv path

for(i in 1:length(a$Project)){
  
  data.list1[[i]]$Project<-a$Project[i]
  data.list1[[i]]$location<-as.character(data.list1[[i]]$location)
  data.list1[[i]]$rating<-as.character(data.list1[[i]]$rating)
  data.list1[[i]]$csv_path<-a$Path[i]
  
}

Names<-NULL
for(i in 1:length(a$Project)){
  Names<-c(Names,names(data.list1[[i]]))
}
table(Names)


# concatenate into one big data.frame and keep only the columns that I want 
OurData <- bind_rows(data.list1) %>% 
  select(Project,location,file,file_duration_sec,start_in_file,end_in_file,duration,year,month,day,hour,minute,second,rating,csv_path) 

OurData$csv_file<- tools::file_path_sans_ext(basename(OurData$csv_path))
head(OurData)
OurData$Savetag<-grepl("(audit_HAPE_21Apr14$|audit_HAPE_21Apr$|_audit_HAPE_21Apr_KJD$|_audit_HAPE_18Jun14$)",OurData$csv_file)
table(OurData$Savetag)

unique(OurData$csv_path[OurData$Savetag==F])

# Read in the NAS scan data and find only the fig files with _audi --------


library(readr)
library(data.table)
nas<-fread(input = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_StreamScan.txt")
figs<- nas %>% filter(str_detect(V2,".fig"))
figs<-figs %>% filter(!str_detect(V2,"_init")) %>% rename(size=V1,path=V2)
head(figs)
figs$fig<-basename(figs$path)

figs$filestream<-basename(dirname(figs$path))
figs$project<-basename(dirname(dirname(figs$path)))

NESH<-figs %>% filter(str_detect(figs$fig,"audit_NESH"))
HAPE<-figs %>% filter(str_detect(figs$fig,"audit_HAPE")) %>% arrange(filestream)

head(HAPE)
