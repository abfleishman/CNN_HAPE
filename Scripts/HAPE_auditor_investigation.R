# This scripe relys on csv log files from log2csv but must have audit_file field
# or will not work.  It also relys on a compiled field data file which is up to 
# date as of 2017-09-06.  

rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

library(lubridate)
library(ggplot2)
library(readr)


# 
# nas<-fread(input = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_FigScan.txt")
# figs<- nas %>% filter(str_detect(V2,".fig"))
# figs<-figs %>% filter(!str_detect(V2,"_init")) %>% rename(size=V1,path=V2) %>% filter(!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",path))
# figs$sub_path<-gsub( '.*Figs/','',figs$path)
# figs$project<-str_extract(figs$sub_path,"[^/]*")
# head(figs)
# figs$fig<-basename(figs$path)
# 
# figs$filestream<-basename(dirname(figs$path))
# # figs$project<-basename(dirname(dirname(figs$path)))
# figs$file<-tools::file_path_sans_ext(basename(figs$path))

# Find Files and project size ---------------------------------------------
a<-data.frame(Path=as.character(list.files(path = "//NAS1/NAS3_2Mar15/All_HAPE",full.names = T,recursive = T)))
# a<-filter(a,str_detect(a$Path,"Log_Files"),str_detect(a$Path,".csv"),!str_detect(a$Path,".jpg")) %>% mutate(Path=as.character(Path))

# add a project
a$Project<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
a$Project<-str_extract(a$Project,"[^/]*$")


a$Species<-"HAPE"

# this goes through all the log files and reads the CSV in to R
data.list1 <- lapply(as.character(a$Path), fread,sep=",")
head(data.list1,1)

# Checks which names are in the the csvs and gives a table of them
Names<-NULL
for(i in 1:length(a$Project)){
  Names<-c(Names,names(data.list1[[i]]))
}
table(Names)

# concatenate into one big data.frame and keep only the columns that I want 
OurData <- bind_rows(data.list1)

# Extract date time info from the sound files because the fig2csv did not work for all files
OurData$file2<-tools::file_path_sans_ext(OurData$file)
OurData$audit_file_fig<-basename(tools::file_path_sans_ext(OurData$audit_file))
OurData$file2<-gsub("_[0-9]{3}$|_[0-9]$","",OurData$file2)
OurData$time<-str_extract(OurData$file2,"[0-9]{6}$")
OurData$date<-str_extract(OurData$file2,"_[0-9]{8}_") %>% str_replace_all("_","") %>% ymd()
OurData$DateTime<-ymd_hms(paste(OurData$date,OurData$time))

OurData$year<-year(OurData$DateTime)
OurData$month<-month(OurData$DateTime)
OurData$day<-day(OurData$DateTime)
OurData$hour<-hour(OurData$DateTime)
OurData$minute<-minute(OurData$DateTime)
OurData$second<-second(OurData$DateTime)
sort(unique(OurData$location))

head(OurData)
hmm<-OurData %>% 
  group_by(year,rating,location) %>%
  filter(rating%in%c(0,4,5)&
         location%in%c("PIHEA1","PIHEA2","PIHEA3","PIHEA4",
                       "ULP1","ULP2","ULP3","ULP4","ULP5","ULP5-","ULP6",
                       "POHK1","POHK2","POHK3","POHK4",
                       "POH1", "POH2","POH3","POH4",
                       "PIHE1","PIHE2","PIHE3","PIHE4" ),!(year%in% c(2000,2011,2012,2013,2014,2015))) %>% 
  summarise(sumRating=n()) %>% 
  arrange(as.numeric(year)) %>% 
  spread(rating,sumRating) %>% 
  rename(pos=`5`,neg=`0`,maybe=`4`)

hmm$pos =ifelse(is.na(hmm$pos),0,hmm$pos)
hmm$neg =ifelse(is.na(hmm$neg),0,hmm$neg)
hmm$maybe =ifelse(is.na(hmm$maybe),0,hmm$maybe)
hmm<-hmm %>% mutate(posNeg=pos/(neg+maybe+pos),
         pos4=maybe/(pos),
         pos0=neg/(pos),
         pos04=(neg+maybe)/pos)

table(hmm$year)

head(hmm)
table(OurData$rating,OurData$year)
ggplot(hmm,aes(x=interaction(year),y=posNeg,color=factor(location)))+geom_point()+theme(axis.text.x = element_text(angle=90))+geom_line(aes(group=location))
ggplot(hmm,aes(x=interaction(year),y=pos4,color=factor(location)))+geom_point()+theme(axis.text.x = element_text(angle=90))+geom_line(aes(group=location))
ggplot(hmm,aes(x=interaction(year),y=pos0,color=factor(location)))+geom_point()+theme(axis.text.x = element_text(angle=90))+geom_line(aes(group=location))
ggplot(hmm,aes(x=interaction(year),y=pos04,color=factor(location)))+geom_point()+theme(axis.text.x = element_text(angle=90))+geom_line(aes(group=location))+ylim(0,10)

ggplot(hmm,aes(x=interaction(year),y=posNeg,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ggplot(hmm,aes(x=interaction(year),y=pos4,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ggplot(hmm,aes(x=interaction(year),y=pos0,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ggplot(hmm,aes(x=interaction(year),y=pos04,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))

ggplot(hmm,aes(x=interaction(year),y=pos,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ggplot(hmm,aes(x=interaction(year),y=neg,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ggplot(hmm,aes(x=interaction(year),y=maybe+neg,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))
ggplot(hmm,aes(x=interaction(year),y=maybe+pos,fill=factor(year)))+geom_boxplot()+theme(axis.text.x = element_text(angle=90))


hmm<-OurData %>% 
  group_by(year,location) %>%
  filter(
           location%in%c("NBOG1","NBOG2","NBOG3","NBOG4","PIHEA1","PIHEA2",
                         "PIHEA3","PIHEA4","ULP1","ULP2","ULP3","ULP4","ULP5","ULP5-","ULP6",
                         "POHK1","POHK2","POHK3","POHK4","POH1", "POH2","POH3","POH4","PIHE1","PIHE2","PIHE3","PIHE4"
           ),!(year%in% c(2000,2011,2012))) 
table(hmm$rating,hmm$year)
