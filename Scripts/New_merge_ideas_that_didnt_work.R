library(dplyr)
library(stringr)
library(data.table)
figs<-read.csv("D:/CM,Inc/CMIAuditor/db/figDb.csv",sep=",",header = FALSE,col.names = c("fig","path"),stringsAsFactors = F) %>% 
  mutate(path1=gsub("^[^/]*/[^/]*/[^/]*",'',path), project=str_split_fixed(gsub("^[/Sounds/]*",'',path1),pattern = "/",n = 2)[,1]) %>%
  filter(str_detect(fig,"mnt"),!str_detect(fig,"Mothership"),!str_detect(fig,"Backup_From_CT_ASUS"),str_detect(fig,"hape|HAPE"))  %>% 
  mutate(filestream=basename(path))

nas<-fread("//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_SoundScan1.txt",sep="\t",header = FALSE,col.names = c("path")) %>% # filter(!str_detect(path,"flac$|wav$")) %>% 
  mutate(path2=gsub("^[^/]*/[^/]*/[^/]*",'',path), project=str_split_fixed(gsub("^[/Sounds/]*",'',path2),pattern = "/",n = 2)[,1]) %>% filter(str_detect(path,"Sounds")) %>% 
  mutate(filestream=basename(path))


head(nas)
head(figs)

miss<-data.frame(table(figs$project[!figs$project%in%unique(nas1$project)]))
missN<-data.frame(table(figs$project[!figs$project%in%unique(nas1$project)]))

library(fuzzyjoin)
hm<-stringdist_left_join(data.frame(project=(miss$Var1)),nas,by="project",max_dist=5,distance_col="dist") %>% group_by(project.x) %>% 
  filter(dist==min(dist)) %>%ungroup %>% 
  mutate(project.y=as.character(project.y),project.x=as.character(project.x),yup=str_detect(hm$project.y, hm$project.x))

table(str_detect(hm$project.y, hm$project.x))

figs %>% filter(project%in%miss$Var1)
nas1 %>% filter(str_detect(path,as.character(miss$Var1[4])))



hmm<-left_join(figs,nas,by=c("path"))
head(hmm)
table(hmm$path1==hmm$path2)
table(is.na(hmm$path2))

nas<-nas %>% 
  mutate(filestream=basename(path))
head(nas)

str_split_fixed(gsub("^[/Sounds/]*",'',head(nas$path)),pattern = "/",n = 2)[,1]

unique(nas$project)[unique(nas$project)%in%unique(gsub("WAV","FLAC",figs$project[str_detect(figs$project,"WAV")]))]
unique(nas$project)[unique(nas$project)%in%unique(figs$project[str_detect(figs$project,"WAV")])]
unique(nas$project)












library(fuzzyjoin)

hmm1<-left_join(figs,nas) %>% filter(is.na(size)) %>% select(-size) %>% stringdist_left_join(nas,by = c("path"),distance_col="dist",max_dist=10)
hmm2<-hmm1 %>% group_by(path.x) %>% filter(dist==min(dist)) 
hmm3 <- left_join(figs,nas) %>% filter(!is.na(size))
head(hmm1)
table(hmm$dist)
