# NESH

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

# Make list of figs to fig2csv
nas<-fread(input = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_FigScan.txt")
figs<- nas %>% filter(str_detect(V2,".fig"))
figs<-figs %>% filter(!str_detect(V2,"_init")) %>% rename(size=V1,path=V2) %>%
  filter(!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",path)) %>%
  filter(grepl("NESH|Nesh|nesh",path))
head(figs)

# if you need to fig2csv more figs later
# figsold<-fread(input = '//NAS1/NAS3_2Mar15/All_HAPE/HAPE2csv.txt', header = F)
# head(figsold)
# figsnew<-filter(figs,!(path %in% figsold$V1))
# # to fig to csv
# writeLines(figsnew$path,'HAPE2csv.txt',sep = "\n")

# to fig to csv
writeLines(figs$path,'NESH2csv.txt',sep = "\n")

figs$sub_path<-gsub( '.*Figs/','',figs$path)
figs$project<-str_extract(figs$sub_path,"[^/]*")
head(figs)
figs$fig<-basename(figs$path)

figs$filestream<-basename(dirname(figs$path))
# figs$project<-basename(dirname(dirname(figs$path)))
figs$file<-tools::file_path_sans_ext(basename(figs$path))
