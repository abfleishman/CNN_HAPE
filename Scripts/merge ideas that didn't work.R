
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

hape_streams<-read_csv("hape_streams.csv")

hape_streams$sub_path<-gsub( '.*Figs\\\\','',hape_streams$audit_file)
hape_streams$sub_path<-gsub( '\\\\','/',hape_streams$sub_path)
hape_streams$project<-str_extract(hape_streams$sub_path,"[^/]*")
hape_streams$fig<-basename(hape_streams$sub_path)
head(hape_streams)

nas<-fread(input = "//NAS1/NAS3_2Mar15/CMI_DataCatalog/NAS_FigScan.txt")
figs<- nas %>% filter(str_detect(V2,".fig"))
figs<-figs %>% filter(!str_detect(V2,"_init")) %>% rename(size=V1,path=V2) %>% filter(!grepl("ToAudit|NoAudit|noAudit|Eval|Comparison|CMI_TrainingData",path))
figs<-figs %>% filter(str_detect(path,"HAPE"))
figs$sub_path<-gsub( '.*Figs/','',figs$path)
figs$project<-str_extract(figs$sub_path,"[^/]*")
head(figs)
figs$fig<-basename(figs$path)

figs$filestream<-basename(dirname(figs$path))
# figs$project<-basename(dirname(dirname(figs$path)))
figs$file<-tools::file_path_sans_ext(basename(figs$path))
head(figs)
hmm<-hape_streams %>% left_join(select(figs,path,fig,project))
