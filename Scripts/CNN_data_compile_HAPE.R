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
a<-filter(a,year%in%2014:2017,Project!="FWS_Rose_Atoll_2014",Species=="HAPE",!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",Path)) 

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
  select(Project,location,file,file_duration_sec,start_in_file,end_in_file,duration,year,month,day,hour,minute,second,rating,csv_path,audit_file) 

OurData$csv_file<- tools::file_path_sans_ext(basename(OurData$csv_path))
head(OurData)
OurData$Savetag<-grepl("(audit_HAPE_21Apr14$|audit_HAPE_21Apr$|_audit_HAPE_21Apr_KJD$|_audit_HAPE_18Jun14$|_audited95_HAPE_21Jun14$)",OurData$csv_file)
table(OurData$Savetag)

unique(OurData$csv_path[OurData$Savetag==F])

OurData <-OurData %>% filter(Savetag==T,!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",csv_path))
OD_figs<-unique(OurData$csv_file)

# Read in the NAS scan data and find only the fig files with _audi --------


library(readr)
library(data.table)
nas<-fread(input = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_FigScan.txt")
figs<- nas %>% filter(str_detect(V2,".fig"))
figs<-figs %>% filter(!str_detect(V2,"_init")) %>% rename(size=V1,path=V2) %>% filter(!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",path))
figs$sub_path<-gsub( '.*Figs/','',figs$path)
figs$project<-str_extract(figs$sub_path,"[^/]*")
head(figs)
figs$fig<-basename(figs$path)

figs$filestream<-basename(dirname(figs$path))
# figs$project<-basename(dirname(dirname(figs$path)))
figs$file<-tools::file_path_sans_ext(basename(figs$path))

NESH<-figs %>% filter(str_detect(figs$fig,"audit_NESH"))
HAPE<-figs %>% filter(file%in%OD_figs) %>% arrange(filestream)

HAPE$round<-gsub("Rovers|rovers|ROV|rov|reaudit|ound_|ound|REFIG|Right","",HAPE$sub_path)
HAPE$round<-str_extract( HAPE$round, "(_[rR].*[[:alnum:]]_)|(/[rR].*[[:alnum:]]/)")
HAPE$round<-gsub("^/_|^/|^_","",HAPE$round)
HAPE$round<-str_extract(HAPE$round,"[^/]*")
HAPE$round<-str_extract(HAPE$round,"([rR][[:alnum:]]{1,})")
HAPE$round<-tolower(HAPE$round)
HAPE$round[is.na(HAPE$round)]<-"r1"
HAPE$year_project<-str_extract(HAPE$project,"([0-9]{4})")
Channel<-gsub("BACKUP|Right|RIGHT|right","RRRRR",HAPE$sub_path)
Channel<-gsub("Left|LEFT|left","LLLLL",Channel)
HAPE$Channel<-"L"
HAPE$Channel<-ifelse(str_detect(Channel,"RRRRR"),"R","L")
HAPE$Project<-str_extract(HAPE$project,"^[a-zA-Z].*[0-9]{4}")
head(HAPE)
write_lines(unique((HAPE$path)),"//NAS1/NAS3_2Mar15/CMI_DataCatalog/Fig2CSV.txt")

a<-data.frame(table(HAPE$file,paste(HAPE$round,HAPE$year_project,HAPE$Channel))) %>% filter(Freq>1)

OurData$year_project<-str_extract(OurData$Project,"([0-9]{4})")

Channel<-gsub("BACKUP|Right|RIGHT|right","RRRRR",OurData$csv_path)
Channel<-gsub("Left|LEFT|left","LLLLL",Channel)
OurData$Channel<-"L"
OurData$Channel<-ifelse(str_detect(Channel,"RRRRR"),"R","L")

OurData$round<-gsub("D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/|Rovers|rovers|ROV|rov|reaudit|ound_|ound|REFIG|Right","",OurData$csv_path)
OurData$round<-str_extract( OurData$round, "(_[rR].*[[:alnum:]]_)|(/[rR].*[[:alnum:]]/)")
OurData$round<-gsub("^/_|^/|^_","",OurData$round)
OurData$round<-str_extract(OurData$round,"[^/]*")
OurData$round<-str_extract(OurData$round,"([rR][[:alnum:]]{1,})")
OurData$round<-tolower(OurData$round)
OurData$round[is.na(OurData$round)]<-"r1"
OurData$Project<-gsub("_reaudit","",OurData$Project)
ODP<-filter(OurData,!is.na(audit_file))
ODR<-filter(OurData,is.na(audit_file))
OurDataH<-left_join(ODR,HAPE,by=c("csv_file"="file","Project","round","Channel","year_project"))


OD_figs<-unique(select(OurData,csv_path,csv_file,Project,year_project,round,Channel))
write_csv(OD_figs,"CSV_paths.csv")

HAPE_figs<-unique(select(HAPE,path,file,Project,year_project,round,Channel))
write_csv(HAPE_figs,"Fig_paths.csv")

OurData_sub<-OurData %>% filter(csv_file%in%unique(HAPE$file[is.na(HAPE$round)])[!unique(HAPE$file[is.na(HAPE$round)])%in%unique(OurData$csv_file[is.na(OurData$round)])])
HAPE_sub<-HAPE %>% filter(file%in%unique(HAPE$file[is.na(HAPE$round)])[!unique(HAPE$file[is.na(HAPE$round)])%in%unique(OurData$csv_file[is.na(OurData$round)])])

KESRP_ULPHONO_2012
OurDataH<-left_join(OurData,HAPE,by=c("csv_file"="file","Project","round","Channel","year_project"))
head(OurDataH)
table(is.na(OurDataH$fig))

ODH_figs<-unique(select(OurDataH,csv_path,csv_file,Project,year_project,round,Channel,path))
write_csv(ODH_figs,"CSVH_paths.csv")
