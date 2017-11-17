rm(list=ls())

library(data.table)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)

## Read in lists of figs (NAS, Auditor, and HAPE model fig list) and sounds from NAS scan (for stream data)
# NAS fig scan
nas<-read_delim(file = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_FigScan.txt", delim = '\t', col_names = c('size', 'FigPath'))
NASfigs<- nas %>% filter(str_detect(FigPath,".fig"))
NASfigs<-NASfigs %>% filter(!str_detect(FigPath,"_init")) %>%
  filter(!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",FigPath)) %>% 
  filter(grepl("HAPE|Hape|hape",FigPath)) %>% 
  select(FigPath)
head(NASfigs)
## NAS sound scan - paired down to streams THIS TAKES A WHILE
NASSounds<-read_delim(file =  "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_SoundScan.txt", delim = '\t', col_names = c('size', 'SoundPath'))
NASStreams<- NASSounds %>%
  filter(str_detect(SoundPath, '[^/]*[.].*$')==F) %>%
  select(SoundPath)
## Auditor FigDB
FigDB<- read_csv(file = "D:/CM,Inc/CMIAuditor/db/figDb.csv",col_names = c('FigPath', 'SoundPath'))
## HAPE mothership fig list
HAPEFigs<- read_csv(file = "//nas1/NAS1_2Jun14/Motherships/HAPE_CNN_Mother_test/hape_streams.csv", col_names = c('FigPath', 'nneg', 'npos', 'outpath', 'outdir'), skip = 1) %>% 
  select(FigPath)
## remove path prefixes
HAPEFigs$FigPath<- str_replace(HAPEFigs$FigPath, '^\\\\\\\\NAS.\\\\', '')
FigDB$FigPath<- str_replace(FigDB$FigPath, '/mnt/', '')
FigDB$OldSoundPath<- FigDB$SoundPath
FigDB$SoundPath<- str_replace(FigDB$SoundPath, '/mnt/', '') 
NASStreams$SoundPath<- str_replace(NASStreams$SoundPath, '/mnt/', '')  
NASfigs$FigPath<- str_replace(NASfigs$FigPath, '/mnt/', '')  
###############################################################
## are any HAPEFigs not in FigDB...... YES, all 8 are _bkp figs. What's up with that?
HAPEFigs_NotInFigDB<- filter(HAPEFigs, !FigPath %in% FigDB$FigPath)
## remove those
HAPEFigs_DB<- filter(HAPEFigs, FigPath %in% FigDB$FigPath)
##############################################################
## find which FigDB figs, that are in HAPE model list, have valid SoundPath, should be good to go
FigDB_HAPE_valid<- FigDB %>% 
  filter(FigPath %in% HAPEFigs_DB$FigPath) %>% 
  filter(SoundPath %in% NASStreams$SoundPath)
###############################################################
## these figs don't have a valid soundpath in FigDB
FigDB_HAPE_invalid<- FigDB %>% 
  filter(FigPath %in% HAPEFigs_DB$FigPath) %>% 
  filter(!SoundPath %in% NASStreams$SoundPath)
## some of these point to SHMOO or and A: or C: drive, figure out project and find data on NAS for these, and make lookup table
FigDB_HAPE_NotNAS<- filter(FigDB_HAPE_invalid, str_detect(SoundPath, '^NAS') ==F)
## save out old path prefix to create OldRoundPath later
FigDB_HAPE_NotNAS$OldPathPrefix<- NA
FigDB_HAPE_NotNAS$OldPathPrefix<- ifelse(is.na(FigDB_HAPE_NotNAS$OldPathPrefix) ==T,  str_extract(FigDB_HAPE_NotNAS$SoundPath, '^.*/Raided_Data_2/'), FigDB_HAPE_NotNAS$OldPathPrefix) 
FigDB_HAPE_NotNAS$OldPathPrefix<- ifelse(is.na(FigDB_HAPE_NotNAS$OldPathPrefix) ==T,  str_extract(FigDB_HAPE_NotNAS$SoundPath, '^.*/Sounds/'), FigDB_HAPE_NotNAS$OldPathPrefix)
FigDB_HAPE_NotNAS$OldPathPrefix<- ifelse(is.na(FigDB_HAPE_NotNAS$OldPathPrefix) ==T,  str_extract(FigDB_HAPE_NotNAS$SoundPath, '^.*/2013/'), FigDB_HAPE_NotNAS$OldPathPrefix)
FigDB_HAPE_NotNAS$SoundPath<- str_replace(FigDB_HAPE_NotNAS$SoundPath, '//', '')  
FigDB_HAPE_NotNAS$SoundPath<- str_replace(FigDB_HAPE_NotNAS$SoundPath, '^[^/]*[^/]*/[^/]*/', '')  
FigDB_HAPE_NotNAS$SoundPath<- str_replace(FigDB_HAPE_NotNAS$SoundPath, 'Sounds/', '')
FigDB_HAPE_NotNAS$SoundPath<- str_replace(FigDB_HAPE_NotNAS$SoundPath, 'WAM_Data_From_Server/', '')  
FigDB_HAPE_NotNAS$SoundPath<- str_replace(FigDB_HAPE_NotNAS$SoundPath, '^2013/', '')  
FigDB_HAPE_NotNAS$Round<- str_extract(FigDB_HAPE_NotNAS$SoundPath, '^[^/]*')
FigDB_HAPE_NotNAS$OldPath_Round<- paste(FigDB_HAPE_NotNAS$OldPathPrefix, FigDB_HAPE_NotNAS$Round, sep = '') 
FigDB_HAPE_NotNAS$OldPathPrefix<-NULL

NotNASRounds<- as.tibble(unique(FigDB_HAPE_NotNAS$Round)) %>% 
  select(OldRound = value) %>% 
  mutate(NewRound = 'Null') 
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='FirstWind_ROV_2013_r1', '', NotNASRounds$NewRound)
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KauaiULP_10Apr14', '', NotNASRounds$NewRound)  
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KESRP_Lehue_2013_R1', 'NAS3_2Mar15/Sounds/KESRP_Lehua_2013_R1_FLAC', NotNASRounds$NewRound)  
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KESRP_Lehua_2014_r2', 'NAS3_2Mar15/Sounds/KESRP_Lehua_2013_R2_FLAC', NotNASRounds$NewRound)  
NotNASRounds$NewRound<- ifelse(NotNASRounds$OldRound =='KIUC_ULP_2013', 'NAS3_2Mar15/Sounds/KESRP_ULPHONO_2013_FLAC', NotNASRounds$NewRound)  
## apply fixes
FigDB_HAPE_NotNAS<- left_join(x = FigDB_HAPE_NotNAS, y = NotNASRounds, by = c('Round'='OldRound'))
############ InvalidRounds need fixing, make lookup table for these
FigDB_HAPE_invalid<- filter(FigDB_HAPE_invalid, str_detect(SoundPath, '^NAS') ==T)
## format data
FigDB_HAPE_invalid$OldPath_Round<- str_extract(FigDB_HAPE_invalid$SoundPath, '^.*/Sounds/[^/]*') 
FigDB_HAPE_invalid$OldPath_Round<- paste('/mnt/', FigDB_HAPE_invalid$OldPath_Round, sep = '')
FigDB_HAPE_invalid$SoundPath<- str_extract(FigDB_HAPE_invalid$SoundPath, 'Sounds/.*')
FigDB_HAPE_invalid$SoundPath<- str_replace(FigDB_HAPE_invalid$SoundPath, 'Sounds/', '')
FigDB_HAPE_invalid$Round<- str_extract(FigDB_HAPE_invalid$SoundPath, '^[^/]*')
## make lookup
InvalidRounds<- as.tibble(unique(FigDB_HAPE_invalid$Round)) %>% 
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
FigDB_HAPE_invalid<- left_join(x = FigDB_HAPE_invalid, y = InvalidRounds, by = c('Round' = 'OldRound'))
#######################################################################################
## bind fixes together and make object for Matlab update
FigDB_HAPE_OldNewPaths1<- rbind(FigDB_HAPE_NotNAS, FigDB_HAPE_invalid)
FigDB_HAPE_OldNewPaths1$FigRound<- str_extract(FigDB_HAPE_OldNewPaths1$FigPath, '^.*/Figs/[^/]*')
FigDB_HAPE_OldNewPaths<- FigDB_HAPE_OldNewPaths1 %>% 
  filter(NewRound != '') %>%
  mutate(NewPath = paste('/mnt/', NewRound, sep = '')) %>%
  group_by(FigRound, OldPath_Round, NewPath) %>% 
  summarise(x = max(SoundPath)) %>% 
  select(FigPath = FigRound, OldPath = OldPath_Round, NewPath)
# ## save out csv
# write_csv(x = FigDB_HAPE_OldNewPaths, path = '//nas1/NAS3_2Mar15/CMI_DataCatalog/archive/FolderMoves/HAPEFigSounds_OldNewPaths.csv', col_names = F)

## make object for changeWavDirBatch command
FigDB_HAPE_ChngWvDirBtch<- FigDB_HAPE_OldNewPaths1 %>% 
  filter(NewRound != '') %>%
  mutate(NewPath = paste('//NAS1/', NewRound, sep = '')) %>%
  group_by(FigRound, OldPath_Round, NewPath) %>% 
  summarise(x = max(SoundPath)) %>% 
  select(FigPath = FigRound, OldPath = OldPath_Round, NewPath)

FigDB_HAPE_ChngWvDirBtch$FigPath<- paste('//NAS1/', FigDB_HAPE_ChngWvDirBtch$FigPath, sep = '')
FigDB_HAPE_ChngWvDirBtch$OldPath<- ifelse(str_detect(FigDB_HAPE_ChngWvDirBtch$OldPath, pattern = '/NAS6_/') ==T, 
                                          str_replace(FigDB_HAPE_ChngWvDirBtch$OldPath, '/mnt/', '//NAS2/') ,str_replace(FigDB_HAPE_ChngWvDirBtch$OldPath, '/mnt/', '//NAS1/'))

FigDB_HAPE_ChngWvDirBtch$FigPath<- gsub( '/', '\\\\', FigDB_HAPE_ChngWvDirBtch$FigPath)
FigDB_HAPE_ChngWvDirBtch$OldPath<- gsub( '/', '\\\\', FigDB_HAPE_ChngWvDirBtch$OldPath)
FigDB_HAPE_ChngWvDirBtch$NewPath<- gsub( '/', '\\\\', FigDB_HAPE_ChngWvDirBtch$NewPath)
