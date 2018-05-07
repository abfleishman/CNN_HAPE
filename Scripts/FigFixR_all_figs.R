rm(list=ls())

library(data.table)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)


# Data Prep ---------------------------------------------------------------

## READ IN NAS FIG SCAN (DONE MANUALLY ON LINUX SERVER)
nas<-read_delim(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_FigScan.txt", delim = '\t', col_names = c('size', 'FigPath'))

# EXTRACT FIG ROUND, NAS, PATH, AND TYPE WHICH IS THE TYPE OF FOLDER (FIGS, SOUNDS, IMAGES)
# FILTER JUST FOR FIGS AND GET UNIQUE ROWS
NASfigs<- nas %>% filter(str_detect(FigPath,".fig$")) %>% 
  select(FigPath) %>% 
  mutate(FigRound=basename(str_extract(FigPath,'/[^/]*[^/]*/[^/]*/[^/]*/[^/]*/')),
         FigNAS=basename(str_extract(FigPath,'/[^/]*[^/]*/[^/]*/')),
         Figtype=basename(str_extract(FigPath,'/[^/]*[^/]*/[^/]*/[^/]*/')),
         FigPath=paste('/mnt',FigNAS,Figtype,FigRound,sep="/")) %>% 
  filter(Figtype=="Figs") %>% 
  distinct()

# unique(NASfigs$FigRound)
# unique(NASfigs$FigNAS)

## READ IN NAS FIG SCAN (DONE MANUALLY ON LINUX SERVER) NAS_SoundScan1.txt is
## directories only not all sounds
NASSounds<-read_delim(file =  "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_SoundScan1.txt", delim = '\t', col_names = c( 'SoundPath'))

# EXTRACT SOUND ROUND, NAS, PATH, AND TYPE WHICH IS THE TYPE OF FOLDER (FIGS, SOUNDS, IMAGES)
# FILTER  GET UNIQUE ROWS and filter for only the project level folders
NASStreams<- NASSounds %>%
  select(SoundPath) %>% 
  mutate(SoundRound=basename(str_extract(SoundPath,'/[^/]*[^/]*/[^/]*/[^/]*/[^/]*/')),
         SoundNAS=basename(str_extract(SoundPath,'/[^/]*[^/]*/[^/]*/')),
         Soundtype=basename(str_extract(SoundPath,'/[^/]*[^/]*/[^/]*/[^/]*/')),
         SoundPath=gsub("/$","",SoundPath)) %>% 
  distinct() %>%
  filter(basename(SoundPath)==SoundRound)

## rEAD IN AUDITOR figDB.csv MADE WITH collectAuditInfo IN MATLAB
## matlab code to create the db
## j1 = submitJob('collectAuditInfo',{'/mnt/NAS1_2Jun14/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS1.csv'});
## j2 = submitJob('collectAuditInfo',{'/mnt/NAS2_9Oct14/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS2.csv'});
## j3 = submitJob('collectAuditInfo',{'/mnt/NAS3_2Mar15/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS3.csv'});
## j4 = submitJob('collectAuditInfo',{'/mnt/NAS4_20Nov15/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS4.csv'});
## j5 = submitJob('collectAuditInfo',{'/mnt/NAS5_6Jun16/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS5.csv'});
## j6 = submitJob('collectAuditInfo',{'/mnt/NAS6_5Jun17/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS6.csv'});
## j7 = submitJob('collectAuditInfo',{'/mnt/NAS7_11Apr18/Figs','/mnt/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS7.csv'});
# 
## tHIS WILL BE A LIST OF FIGDB.CSV FILES ONE FOR EACH NAS
## EXTRACT SOUND AND FIG INFO (TRICKIER SINCE NOT AL LTHE NAMES ARE SAME FORMAT)
## FOR NAS I made a sting with all the possible options including leter drive names.
## 
FigDB<- read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS1.csv",col_names = c('FigPathDB', 'SoundPathDB')) %>% 
  bind_rows(read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS2.csv",col_names = c('FigPathDB', 'SoundPathDB'))) %>% 
  bind_rows(read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS3.csv",col_names = c('FigPathDB', 'SoundPathDB'))) %>% 
  bind_rows(read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS4.csv",col_names = c('FigPathDB', 'SoundPathDB'))) %>% 
  bind_rows(read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS5.csv",col_names = c('FigPathDB', 'SoundPathDB'))) %>% 
  bind_rows(read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS6.csv",col_names = c('FigPathDB', 'SoundPathDB'))) %>% 
  bind_rows(read_csv(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/figDB_NAS7.csv",col_names = c('FigPathDB', 'SoundPathDB'))) %>% 
  mutate(FigRound=basename(str_extract(FigPathDB,'/[^/]*[^/]*/[^/]*/[^/]*/[^/]*/')),
         FigNAS=basename(str_extract(FigPathDB,'^A|^C|^D|^E|^F|home|/data2|^V|STRIKER|CMI_01-PC|NOT FOUND|SHMOO|shmoo|NAS1_2Jun14|NAS2_9Oct14|NAS3_2Mar15|NAS4_20Nov15|NAS5_6Jun16|NAS6_5Jun17|NAS7_11Apr18')),
         Figtype=basename(str_extract(FigPathDB,'/[^/]*[^/]*/[^/]*/[^/]*/')),
         SoundNAS=basename(str_extract(SoundPathDB,'^A|^C|^D|^E|^F|home|/data2|^V|STRIKER|CMI_01-PC|NOT FOUND|SHMOO|shmoo|NAS1_2Jun14|NAS2_9Oct14|NAS3_2Mar15|NAS4_20Nov15|NAS5_6Jun16|NAS6_5Jun17|NAS7_11Apr18')),
         Soundtype=basename(str_extract(SoundPathDB,'/[^/]*[^/]*/[^/]*/[^/]*/')),
         SoundPathDB=gsub("/$","",SoundPathDB)) %>% 
  filter(Figtype=="Figs",!str_detect(FigPathDB,"Mothership") )


# Make the Sound Round.  To do this I scaned the list of files that did not match up manually down below
FigDB$SoundRound<-FigDB$SoundPathDB %>% 
  gsub('^A|^C|^D|^E|^F|home|/data2|^V|STRIKER|CMI_01-PC|NOT FOUND|SHMOO|shmoo|NAS1_2Jun14|NAS2_9Oct14|NAS3_2Mar15|NAS4_20Nov15|NAS5_6Jun16|NAS6_5Jun17|NAS7_11Apr18',"",.) %>% 
  gsub('/mnt/|\\\\NAS1|\\\\NAS2',"",.) %>% 
  gsub('^/Sounds/|^/Figs/',"",.) %>% 
  gsub('^FLACs/|\\:/Project_Data/2013/|\\:/Project_Data/|///2014_Single_1/Sounds/',"",.) %>% 
  gsub('WAM_Data_From_Server/|^///Raided_Data_2/|\\\\NAS2//Sounds/|\\\\NAS1////Sounds//|From_Abe/SEFI/|^/|\\\\/Project_Data/2013/|/NAS2//Sounds/|/NAS1////Sounds//|\\\\Project_Data/2013/|\\:/',"",.) %>% 
  gsub('\\\\Project_Data/2013/|//Project_Data/2013/|^\\.$',"",.) %>% 
  str_extract('[^/]*[^/]')
  

unique(FigDB$SoundRound)
table(is.na(FigDB$SoundRound))
FigDB %>% filter(is.na(SoundRound))
FigDB %>% filter(SoundRound=="CM")
# Manually dropping the NZSP_2014 and some Test figs and get rid of anything that SoundRound==NA
FigDB<-FigDB %>% filter(SoundRound!="CM")

# Join the FigDB and the Nasfig scan and remove any files that are not in the DB
# since there is not anything we can do about them
FigDB<-FigDB %>% full_join(NASfigs) 

Figs_NotInFigDB<- FigDB %>% filter(is.na(FigPathDB))
FigDB<- FigDB %>% filter(!is.na(FigPathDB))

# make a noWav/Flac round to match the DB with the NASStreams
FigDB$SoundRound_noflac<-gsub('_FLAC|_WAV|_Flac|_flac|_fLAC|_Wav|_wav|_wAV','',FigDB$SoundRound)
NASStreams$SoundRound_noflac<-gsub('_FLAC|_WAV|_Flac|_flac|_fLAC|_Wav|_wav|_wAV','',NASStreams$SoundRound)

NASStreams<-NASStreams %>% rename(SoundRound_NAS=SoundRound,Soundtype_NAS=Soundtype,SoundNAS_NAS=SoundNAS)
FigDB<-FigDB %>% rename(SoundRound_Fig=SoundRound,Soundtype_Fig=Soundtype,SoundNAS_Fig=SoundNAS)

# Join the DB to the Streams
FigDB_sound_join <- FigDB %>% full_join(NASStreams,by=c("SoundRound_noflac")) %>% 
  mutate(Sounds_OK=str_detect(SoundPathDB,SoundPath))
unique(FigDB_sound_join$Sounds_OK)

# Find missing links ------------------------------------------------------

# Filter the mismatches
# These are all the fig files that the sound dir in the database did not match the sound folders on the NAS
FigDB_valid_sounds<-FigDB_sound_join %>%
  filter(Sounds_OK%in%c(T),!str_detect(SoundPathDB,"TrainingSounds"))


FigDB_invalid_sounds_that_join<-FigDB_sound_join %>%
  filter(Sounds_OK%in%c(F),!str_detect(SoundPathDB,"TrainingSounds"))

FigDB_invalid_sounds<-FigDB_sound_join %>%
  filter(is.na(Sounds_OK),!str_detect(SoundPathDB,"TrainingSounds"))

# Find all the figDirs that do not have a valid sound dir
# Save these out  to manually evaluate
FigDB_invalid_sounds_1row<-FigDB_sound_join %>%
  filter(is.na(Sounds_OK),!str_detect(SoundPathDB,"TrainingSounds")) %>%
  group_by(SoundRound_Fig) %>%
  filter(row_number()==1) # get the first row only

# Save out for manual edits
# FigDB_invalid_sounds_1row %>% write.csv("sound_lookup_7May18.csv")




### MANUAL EDITS REQUIRED BELOW HERE!!!!! #####
### 
### 
### 
### 
# Read the manual edits back in (NewPath is the new sound dir)
FigDB_invalid_sounds_edits<-read_csv("sound_lookup_2May18_Edited.csv")%>%
  # filter(!NewPath%in%c("REMOVE","NOT_DB","GLACIER","MULTI")) %>% 
  filter(!str_detect(SoundPathDB,"TrainingSounds")) %>%
  mutate(NewPath=gsub('\\\\','/',NewPath),
         NewPath=gsub('//NAS1/','/mnt/',NewPath)) %>% 
  select(NewPath,FigRound,FigNAS,Figtype, SoundNAS_Fig, Soundtype_Fig, SoundRound_Fig ) %>%
  distinct()  


a<-FigDB_invalid_sounds %>% left_join(FigDB_invalid_sounds_edits)

a$SoundPath<-ifelse(is.na(a$SoundPath)&!a$NewPath%in%c("REMOVE","NOT_DB","GLACIER","MULTI"),a$NewPath,a$SoundPath)

b<-a %>%
  bind_rows(FigDB_invalid_sounds_that_join) %>% 
  filter(!is.na(SoundPath),!str_detect(SoundPathDB,"DNN_")) %>%
  select(FigPathDB,SoundPathDB,SoundPathNew=SoundPath) %>% 
  mutate(OldString=gsub("/mnt/",'',SoundPathDB),
         OldString=gsub("/NW1/Slot B|/NW2/Slot B|/NW1/Slot A|/NW2/Slot A|/Batch1|/Batch2","",OldString),
         NewString=gsub("/mnt/","",SoundPathNew))

b$NewString[str_detect(b$OldString,"CMI_01|shmoo|SHMOO|E:/")]<-paste0("/mnt/",b$NewString[str_detect(b$OldString,"CMI_01|shmoo|SHMOO|E:/")])

unique(b$NewString)
b$OldString[!dirname(b$OldString)%in%c("NAS2_9Oct14/Sounds","NAS5_6Jun16/Sounds","E:/Project_Data"  )]<-dirname(b$OldString[!dirname(b$OldString)%in%c("NAS2_9Oct14/Sounds","NAS5_6Jun16/Sounds" ,"E:/Project_Data" )])
unique((b$OldString))

b$FigDir[dirname(dirname(b$FigPathDB))%in%c("/mnt/NAS5_6Jun16/Figs" ,"/mnt/NAS1_2Jun14/Figs"  ,"/mnt/NAS3_2Mar15/Figs")]<-b$FigPathDB[dirname(dirname(b$FigPathDB))%in%c("/mnt/NAS5_6Jun16/Figs" ,"/mnt/NAS1_2Jun14/Figs"  ,"/mnt/NAS3_2Mar15/Figs")]
b$FigDir[!dirname(dirname(b$FigPathDB))%in%c("/mnt/NAS5_6Jun16/Figs" ,"/mnt/NAS1_2Jun14/Figs"  ,"/mnt/NAS3_2Mar15/Figs")]<-dirname(dirname(b$FigPathDB))[!dirname(dirname(b$FigPathDB))%in%c("/mnt/NAS5_6Jun16/Figs" ,"/mnt/NAS1_2Jun14/Figs"  ,"/mnt/NAS3_2Mar15/Figs")]

unique(b$FigDir)
b$FigPathDB
b %>% select(FigDir,OldString,NewString) %>% filter(!str_detect(OldString,"NAS[12]/"))%>%
  distinct %>% 
  # mutate(OldString=ifelse(OldString=="NAS1_2Jun14/Sounds/KESRP_HNKP_2015_R2_FLAC","NAS1_2Jun14/Sounds/KESRP_HNKP_2015_R2_FLAC_SPLIT_SPLIT",OldString)) %>%
  write.csv("//NAS1/NAS3_2Mar15/CMI_DataCatalog/FigSoundPathRepair/FigSoundPathFix_2May18.csv",row.names = F)
b %>% select(FigDir,OldString,NewString) %>% filter(!str_detect(OldString,"NAS[12]/"))%>%
  distinct %>% View
# FigDir, SoundDirDB, ReplaceString, wav, flac
# These are the valid sounds
FigDB_valid_sounds<-FigDB_sound_join %>% filter(!is.na(SoundPath))

# These are the invalid sounds that are listed as not on the NAS??? This might not be right FIX
FigDB_sounds_NotNas<- FigDB_invalid_sounds %>% filter(!str_detect(SoundNAS,"NAS"))

# For fixing in matlab but things are not working
# 
# cd 'D:\CM,Inc\CMIAuditor'
# %%
#   mainDir='/mnt/NAS1_2Jun14/Figs/IC_Rayadito_2014';
# [dirNames fullPaths] = findMatch(convertPath(mainDir),'.fig');
# f=1;
# for f = 1:length(fullPaths)
# [dirName,figName,ext] = fileparts(fullPaths{f});
# if strfind(figName,'_init')
# continue
# %    elseif strfind(figName,'Lehua')
# %        continue
# else
#   auditor(dirName,figName)
# auditorSaveProject(dirName,figName)
# delete(gcf);
# end
# end
# 
# changeWavDirBatch(convertPath(mainDir),...
#                   convertPath('\\SHMOO/2014_Single_1/Sounds/IC_Rayadito_2014'),...
#                   convertPath('/mnt/NAS1_2Jun14/Sounds/IC_Rayadito_2014'),'','flac', 'wav')
# 
# 
# 






# Jeffs code below here.  Might need for next steps








###############################################################
## are any Figs not in FigDB...... YES, all 8 are _bkp figs. What's up with that?
Figs_NotInFigDB<- filter(NASfigs, !FigPath %in% FigDB$FigPath)
## remove those
Figs_DB<- filter(NASfigs, FigPath %in% FigDB$FigPath)

##############################################################
## find which FigDB figs, that are in HAPE model list, have valid SoundPath, should be good to go
FigDB_valid<- FigDB %>% 
  filter(FigPath %in% Figs_DB$FigPath) %>% 
  filter(SoundPath %in% NASStreams$SoundPath)

###############################################################
## these figs don't have a valid soundpath in FigDB
FigDB_invalid<- FigDB %>% 
  filter(FigPath %in% Figs_DB$FigPath) %>% 
  filter(!SoundPath %in% NASStreams$SoundPath)
saveRDS(FigDB_invalid,"broken_figs_1May18.rds")
## some of these point to SHMOO or and A: or C: drive, figure out project and find data on NAS for these, and make lookup table
FigDB_NotNAS<- filter(FigDB_invalid, str_detect(SoundPath, '^NAS') ==F)

## save out old path prefix to create OldRoundPath later
FigDB_NotNAS$OldPathPrefix<- NA
FigDB_NotNAS$OldPathPrefix<- ifelse(is.na(FigDB_NotNAS$OldPathPrefix) ==T,  str_extract(FigDB_NotNAS$SoundPath, '^.*/Raided_Data_2/'), FigDB_NotNAS$OldPathPrefix) 
FigDB_NotNAS$OldPathPrefix<- ifelse(is.na(FigDB_NotNAS$OldPathPrefix) ==T,  str_extract(FigDB_NotNAS$SoundPath, '^.*/Sounds/'), FigDB_NotNAS$OldPathPrefix)
FigDB_NotNAS$OldPathPrefix<- ifelse(is.na(FigDB_NotNAS$OldPathPrefix) ==T,  str_extract(FigDB_NotNAS$SoundPath, '^.*/2013/'), FigDB_NotNAS$OldPathPrefix)
FigDB_NotNAS$SoundPath<- str_replace(FigDB_NotNAS$SoundPath, '//', '')  
FigDB_NotNAS$SoundPath<- str_replace(FigDB_NotNAS$SoundPath, '^[^/]*[^/]*/[^/]*/', '')  
FigDB_NotNAS$SoundPath<- str_replace(FigDB_NotNAS$SoundPath, 'Sounds/', '')
FigDB_NotNAS$SoundPath<- str_replace(FigDB_NotNAS$SoundPath, 'WAM_Data_From_Server/', '')  
FigDB_NotNAS$SoundPath<- str_replace(FigDB_NotNAS$SoundPath, '^2013/', '')  
FigDB_NotNAS$Round<- str_extract(FigDB_NotNAS$SoundPath, '^[^/]*')
FigDB_NotNAS$OldPath_Round<- paste(FigDB_NotNAS$OldPathPrefix, FigDB_NotNAS$Round, sep = '') 
FigDB_NotNAS$OldPathPrefix<-NULL

NotNASRounds<- as.tibble(unique(FigDB_NotNAS$Round)) %>% 
  select(OldRound = value) %>% 
  mutate(NewRound = 'Null') 
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='FirstWind_ROV_2013_r1', '', NotNASRounds$NewRound)
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KauaiULP_10Apr14', '', NotNASRounds$NewRound)  
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KESRP_Lehue_2013_R1', 'NAS3_2Mar15/Sounds/KESRP_Lehua_2013_R1_FLAC', NotNASRounds$NewRound)  
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KESRP_Lehua_2014_r2', 'NAS3_2Mar15/Sounds/KESRP_Lehua_2013_R2_FLAC', NotNASRounds$NewRound)  
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KIUC_ULP_2013', 'NAS3_2Mar15/Sounds/KESRP_ULPHONO_2013_FLAC', NotNASRounds$NewRound)  

## apply fixes
FigDB_NotNAS<- left_join(x = FigDB_NotNAS, y = NotNASRounds, by = c('Round'='OldRound'))

############ InvalidRounds need fixing, make lookup table for these
FigDB_invalid<- filter(FigDB_invalid, str_detect(SoundPath, '^NAS') ==T)

## format data
FigDB_invalid$OldPath_Round<- str_extract(FigDB_invalid$SoundPath, '^.*/Sounds/[^/]*') 
FigDB_invalid$OldPath_Round<- paste('/mnt/', FigDB_invalid$OldPath_Round, sep = '')
FigDB_invalid$SoundPath<- str_extract(FigDB_invalid$SoundPath, 'Sounds/.*')
FigDB_invalid$SoundPath<- str_replace(FigDB_invalid$SoundPath, 'Sounds/', '')
FigDB_invalid$Round<- str_extract(FigDB_invalid$SoundPath, '^[^/]*')

## make lookup
InvalidRounds<- as.tibble(unique(FigDB_invalid$Round)) %>% 
  select(OldRound = value) %>% 
  mutate(NewRound = 'Null') 
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='IC_Pinzon_2012', 'NAS2_9Oct14/Sounds/IC_Pinzon_2012_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_HNKP_2015_R2_FLAC', 'NAS1_2Jun14/Sounds/KESRP_HNKP_2015_R2_FLAC_SPLIT', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_HNKP_2015_R2_FLAC_RIGHT', 'NAS1_2Jun14/Sounds/KESRP_HNKP_2015_R2_FLAC_SPLIT_BACKUP', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2013', 'NAS3_2Mar15/Sounds/KESRP_ULPHONO_2013_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='FIWI_2014_r1', 'NAS3_2Mar15/Sounds/FIWI_2014_R1_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2014_R3', 'NAS1_2Jun14/Sounds/KESRP_ULPHONO_2014_R3_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2014_R4', 'NAS1_2Jun14/Sounds/KESRP_ULPHONO_2014_R4_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2014_R5', 'NAS1_2Jun14/Sounds/KESRP_ULPHONO_2014_R5_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2014_R6', 'NAS1_2Jun14/Sounds/KESRP_ULPHONO_2014_R6_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2014_R7', 'NAS2_9Oct14/Sounds/KESRP_ULPHONO_2014_R7_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULPHONO_2014_R8', 'NAS1_2Jun14/Sounds/KESRP_ULPHONO_2014_R8_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_ULP_2012', 'NAS3_2Mar15/Sounds/KESRP_ULP_2012_FLAC', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='KESRP_UMP_2014_R3_WAV_BACKUP', 'NAS2_9Oct14/Sounds/KESRP_UMP_2014_R3_FLAC_BACKUP', InvalidRounds$NewRound)  
InvalidRounds$NewRound<- ifelse(InvalidRounds$OldRound =='MNSRP_Kahikinui_2014_r1_WAV_22050', 'NAS3_2Mar15/Sounds/MNSRP_Kahikinui_2014_R1_FLAC', InvalidRounds$NewRound) 

## apply lookup fixes
FigDB_invalid<- left_join(x = FigDB_invalid, y = InvalidRounds, by = c('Round' = 'OldRound'))

#######################################################################################
## bind fixes together and make object for Matlab update
FigDB_OldNewPaths1<- rbind(FigDB_NotNAS, FigDB_invalid)
FigDB_OldNewPaths1$FigRound<- str_extract(FigDB_OldNewPaths1$FigPath, '^.*/Figs/[^/]*')
FigDB_OldNewPaths<- FigDB_OldNewPaths1 %>% 
  filter(NewRound != '') %>%
  mutate(NewPath = paste('/mnt/', NewRound, sep = '')) %>%
  group_by(FigRound, OldPath_Round, NewPath) %>% 
  summarise(x = max(SoundPath)) %>% 
  select(FigPath = FigRound, OldPath = OldPath_Round, NewPath)
# ## save out csv
# write_csv(x = FigDB_OldNewPaths, path = '//nas1/NAS3_2Mar15/CMI_DataCatalog/archive/FolderMoves/HAPEFigSounds_OldNewPaths.csv', col_names = F)

## make object for changeWavDirBatch command
FigDB_ChngWvDirBtch<- FigDB_OldNewPaths1 %>% 
  filter(NewRound != '') %>%
  mutate(NewPath = paste('//NAS1/', NewRound, sep = '')) %>%
  group_by(FigRound, OldPath_Round, NewPath) %>% 
  summarise(x = max(SoundPath)) %>% 
  select(FigPath = FigRound, OldPath = OldPath_Round, NewPath)

FigDB_ChngWvDirBtch$FigPath<- paste('//NAS1/', FigDB_ChngWvDirBtch$FigPath, sep = '')
FigDB_ChngWvDirBtch$OldPath<- ifelse(str_detect(FigDB_ChngWvDirBtch$OldPath, pattern = '/NAS6_/') ==T, 
                                          str_replace(FigDB_ChngWvDirBtch$OldPath, '/mnt/', '//NAS2/') ,str_replace(FigDB_ChngWvDirBtch$OldPath, '/mnt/', '//NAS1/'))

FigDB_ChngWvDirBtch$FigPath<- gsub( '/', '\\\\', FigDB_ChngWvDirBtch$FigPath)
FigDB_ChngWvDirBtch$OldPath<- gsub( '/', '\\\\', FigDB_ChngWvDirBtch$OldPath)
FigDB_ChngWvDirBtch$NewPath<- gsub( '/', '\\\\', FigDB_ChngWvDirBtch$NewPath)
