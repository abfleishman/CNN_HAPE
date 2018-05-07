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
  filter(!grepl("(ToAudit|NoAudit|noAudit|Eval|Comparison|compare|Comp|comp|conflict|CMI_TrainingData|CMI_FigPlayground_2017|_bkp|_HAPEtone_Rated)",path)) %>%
  mutate(folder=dirname(dirname(path)))
head(figs)

# figsCHICKS<-figs %>% filter(grepl("CHICK|Chick|chick",path))
# unique(figsCHICKS$folder)

figsHAPE<-figs %>% filter(grepl("HAPE|Hape|hape",path))
figsNESH<-figs %>% filter(grepl("NESH|Nesh|nesh",path))
figsBANP<-figs %>% filter(grepl("BANP|Banp|banp",path))
figsBAOW<-figs %>% filter(grepl("BAOW|Baow|baow",path))
figsWTSH<-figs %>% filter(grepl("WTSH|Wtsh|wtsh",path))
figsBUPE<-figs %>% filter(grepl("BUPE|Bupe|bupe",path))

figsOTHER<-filter(figs,!(path %in% c(figsHAPE$path,figsNESH$path,figsBANP$path,figsBAOW$path)))

# if you need to fig2csv more figs later
# HAPE
figsHAPEold<-fread(input = '//NAS1/NAS3_2Mar15/All_HAPE/HAPE2csv.txt', header = F)
head(figsHAPEold)
figsHAPEnew<-filter(figsHAPE,!(path %in% figsHAPEold$V1))
# to fig to csv
writeLines(figsHAPEnew$path,'HAPE2csv_more_20Feb18.txt',sep = "\n")

# to fig to csv
writeLines(figsNESH$path,'NESH2csv.txt',sep = "\n")
writeLines(figsBANP$path,'BANP2csv.txt',sep = "\n")
writeLines(figsBAOW$path,'BAOW2csv.txt',sep = "\n")
writeLines(figsWTSH$path,'WTSH2csv.txt',sep = "\n")
writeLines(figsBUPE$path,'BUPE2csv.txt',sep = "\n")
