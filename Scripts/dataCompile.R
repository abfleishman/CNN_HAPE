
library(stringr)
library(tidyverse)
library(data.table)
library(lubridate)

rm(list=ls())
gc()

# HAPE

# Find Files and project size ---------------------------------------------
HAPElogfiles<-data.frame(Path=as.character(list.files(path = "//NAS1/NAS3_2Mar15/All_HAPE/mnt",full.names = T,recursive = T)))
# HAPElogfiles<-filter(HAPElogfiles,str_detect(HAPElogfiles$Path,"Log_Files"),str_detect(HAPElogfiles$Path,".csv"),!str_detect(HAPElogfiles$Path,".jpg")) %>% mutate(Path=as.character(Path))

# add info about the project/fig
HAPElogfiles$Path<-as.character(HAPElogfiles$Path)
HAPElogfiles$RoundFolder<-str_extract(str_extract(HAPElogfiles$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*"),"[^/]*$")
HAPElogfiles$Project<-str_replace(str_replace(str_replace(HAPElogfiles$RoundFolder,'_BACKUP',''),'_(R|r)[0-9]{1,2}.*$',''),'_DateFiltered','')
HAPElogfiles$ProjectYear<-str_extract(HAPElogfiles$Project,"[0-9]{4}")
HAPElogfiles$Species<-"HAPE"
head(HAPElogfiles)
HAPElogfiles$DirName<-str_extract(str_extract(HAPElogfiles$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*"),"[^/]*$")
HAPElogfiles$AuditFigName<-str_replace(basename(HAPElogfiles$Path),'.csv','')
HAPElogfiles$BaseFigName<-str_replace(HAPElogfiles$AuditFigName,'(_audit.*$|audit.*$|_dnnClass.*$|_all95.*$|_MMaudit.*$)','')
HAPElogfiles$BaseFigPath<-paste0(dirname(HAPElogfiles$Path),'/',HAPElogfiles$BaseFigName,'.fig')

# check out how many figs are in weird folder structures
hm<-HAPElogfiles[which(HAPElogfiles$BaseFigName!=HAPElogfiles$DirName),]
head(hm)

# # reconstruct base fig path and check for duplicates
# sum(duplicated(HAPElogfiles$BaseFigPath))
# dups<-HAPElogfiles[duplicated(HAPElogfiles$BaseFigPath),]
# dups<-bind_rows(dups,HAPElogfiles[duplicated(HAPElogfiles$BaseFigPath,fromLast=T),])
# dups<-filter(dups,!duplicated(Path))
# dups$keep=NA
# dups$keep[str_detect(dups$AuditFigName,'(_DS_)|(_DTS_)')]=0
# dups$keep[str_detect(dups$AuditFigName,'_all95_')]=0
# write.csv(dups,'HAPEDuplicateLogs.csv')
# rm(dups)

dups<-read.csv('HAPEDuplicateLogs_edited.csv')
# take out duplicates
HAPElogfiles<-filter(HAPElogfiles,!(Path %in% dups$Path[dups$keep==0]))

# this goes through all the log files and reads the CSV in to R
data.list1 <- lapply(as.character(HAPElogfiles$Path), fread,sep=",")
head(data.list1,1)

# Checks which names are in the the csvs and gives a table of them
Names<-NULL
for(i in 1:length(HAPElogfiles$Project)){
  Names<-c(Names,names(data.list1[[i]]))
}
table(Names)

# concatenate into one big data.frame and keep only the columns that I want 
OurData <- bind_rows(data.list1)

# extract fig name from full audit file path
OurData$audit_file_fig<-basename(tools::file_path_sans_ext(OurData$audit_file))

# # Extract date time info from the sound files because the fig2csv did not work for all files
# # Don't really need this...
# OurData$file2<-tools::file_path_sans_ext(OurData$file)
# OurData$time<-str_extract(OurData$file2,"([0-9]{6}$)|([0-9]{6}_000$)|([0-9]{6}_0$)|([0-9]{6}_1$)")
# OurData$date<-str_extract(OurData$file2,"_[0-9]{8}_") %>% str_replace_all("_","") %>% ymd()
# OurData$DateTime<-ymd_hms(paste(OurData$date,OurData$time))
# OurData$year<-year(OurData$DateTime)
# OurData$month<-month(OurData$DateTime)
# OurData$day<-day(OurData$DateTime)
# OurData$hour<-hour(OurData$DateTime)
# OurData$minute<-minute(OurData$DateTime)
# OurData$second<-second(OurData$DateTime)
# head(OurData)
# # adjust datetime for P rows?

# read in SPID_Table from the FieldData_Compile.r -------------------------

SPData<-read_csv("SPID_Table.csv") %>% mutate(deploy=mdy_hms(paste(Deployment_Date, Deployment_Time)),retrival=mdy_hms(paste(Retrieval_Date, Retrieval_Time))) %>% filter(!is.na(deploy),!is.na(retrival))%>% data.table()
head(SPData)

# Fix a weird record where the unit was "deployed" after retieval
SPData$retrival[which(SPData$deploy>SPData$retrival)]<-SPData$retrival[which(SPData$deploy>SPData$retrival)]+years(1)

# Use data table to join by time index and sensor name
setkey(SPData, Sensor_Name,deploy, retrival)

OurData$Sensor_Name<-OurData$location
OurData<-OurData %>% data.table()
OurData$DateTime2<- OurData$DateTime

dat<-foverlaps(OurData,SPData,by.x=c("Sensor_Name","DateTime","DateTime2"),mult="first")
head(dat)
dat<-dat %>% dplyr::select(Sensor_Name,location,SPID, Project,  Latitude, Longitude, file, id,probeId,audit_file, file_duration_sec,
                           start_in_file, end_in_file, duration, rating, DateTime, date, year, month, day,hour, minute, second,
                           flux, flux_sensitive, burst,level, level_absolute, click)
head(dat)

dat2<-dat %>% filter(is.na(dat$Latitude))

a<-data.frame(table(dat2$location,dat2$year)) %>% filter(Freq>100) %>% arrange(Freq)

# Make a grid -------------------------------------------------------------

library(sp)
library(proj4)
# library(raster)
library(adehabitatMA)

map_table<-dat %>% dplyr::select(Latitude,Longitude,SPID,Project) %>% distinct() %>% filter(!is.na(Longitude))

map_table.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(as.numeric(map_table$Longitude),as.numeric(map_table$Latitude))),
                                         data=map_table,
                                         proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
mean(map_table.spdf@bbox[1,])
mean(map_table.spdf@bbox[2,])
# Project data into laea
map_table.spdf.t <- spTransform(map_table.spdf,
                                CRS("+proj=laea +lat_0=20.69025 +lon_0=-157.8788 
                                    +ellps=WGS84 +units=km +no_defs"))

map_table.spdf.t$x_laea<-round(coordinates(map_table.spdf.t)[,1],0)
map_table.spdf.t$y_laea<-round(coordinates(map_table.spdf.t)[,2],0)

# Regular grid with no land mask ---------------------------
# define the coordinates (all the detections fall inside the polygon)
# 2. Compute a grid around the fixes.
buffer_x <- as.integer((max(map_table.spdf.t$x_laea) - min(map_table.spdf.t$x_laea)) * 0.5/100)
buffer_y <- as.integer((max(map_table.spdf.t$y_laea) - min(map_table.spdf.t$y_laea)) * 0.5/100)
buffer <- max(buffer_x, buffer_y)
xy_sp <- SpatialPoints(data.frame(x = c((as.integer((max(map_table.spdf.t$x_laea) + 100)/100) * 100 + buffer),
                                        (as.integer((min(map_table.spdf.t$x_laea) - 100)/100) * 100 - buffer)),
                                  y = c((as.integer((max(map_table.spdf.t$y_laea) + 100)/100) * 100 + buffer),
                                        (as.integer((min(map_table.spdf.t$y_laea) - 100)/100) * 100 - buffer))))
customGrid50 <- ascgen(xy_sp, cellsize = 7.5) #in km


customGrid50$x<-1:length(customGrid50)

raster::crs(customGrid50)<-CRS("+proj=laea +lat_0=20.69025 +lon_0=-157.8788 +ellps=WGS84 +units=km +no_defs")
plot(customGrid50)
plot(map_table.spdf.t,add=T)


map_table.spdf.t$grid<-over(map_table.spdf.t,customGrid50)$x

dat1<-left_join(map_table,as.data.frame(map_table.spdf.t)) %>%
  distinct() %>%
  dplyr::select(Latitude,Longitude, SPID,Project,grid) %>%
  right_join(dat)

table(dat1$audit_file,dat1$rating)


# Identify and remove filestreams with all windows not 5s rated  zero  --------

hmm<-data.frame(table(dat1$audit_file,dat1$rating))

dat1 <- dat1 %>% filter(!audit_file%in%as.character(hmm$Var1[hmm$Freq>100000]))

# select a random subset of events ----------------------------------------

# num rows for each rating in each grid cell
nGrid<-data.frame(table(dat1$grid,dat1$rating))
# nGrid<-data.frame(table(dat1$SPID,dat1$rating))

# select only the positives and the number of per grid so get ~5000
posL<-floor(10000/(nGrid %>% filter(Var2=="5",Freq>102) %>% arrange(Freq) %>% nrow()) )

# select only the p rows and the number of per grid so get ~50000
negL<-floor(100000/(nGrid %>% filter(Var2=="P",Freq>1500) %>% arrange(Freq) %>% nrow()) )

set.seed(1) # for randomization and reproducibility

# filter 5s group by grid filter for the grids that have > posL and sample posL
pos1<-dat1 %>% 
  filter(rating==5) %>% 
  group_by(grid) %>% 
  filter(n()>posL) %>% 
  sample_n(posL)

# get all rows at sites with < posL
pos2<-dat1 %>% 
  filter(rating==5) %>% 
  group_by(grid) %>% 
  filter(n()<=posL)

hapePos<-bind_rows(pos1,pos2) %>%
  mutate(review="HAPE_CNN_Mother_test")%>% #review column for the exportDulplicateEvnets is maybe not needed
  dplyr::select(Project,id,probeId,file,start_in_file,end_in_file,audit_file,SPID,grid,review)

# write.csv(hapePos,"HAPE_CNN_Mother_test.csv",row.names = F)

hapePos_streams<-hapePos %>% group_by(audit_file) %>% summarise(npos=n())

hapeNeg<-dat1 %>% 
  filter(rating=="P") %>% 
  group_by(grid) %>% 
  filter(n()>negL) %>%
  sample_n(negL)



# make up some start times *** this is not generalized for when longer recordings are pressent
durs<-seq(from=0,to=58,by=2)

# hapeNeg1<-hapeNeg %>% mutate(start_in_file=sample(durs,replace = T,size=n()),
#                             start_in_file=ifelse(start_in_file+2>duration,sample(durs,replace = T,size=n()),start_in_file),
#                             start_in_file=ifelse(start_in_file+2>duration,sample(durs,replace = T,size=n()),start_in_file),
#                             start_in_file=ifelse(start_in_file+2>duration,sample(durs,replace = T,size=n()),start_in_file),
#                             start_in_file=ifelse(start_in_file+2>duration,sample(durs,replace = T,size=n()),start_in_file),
#                             start_in_file=ifelse(start_in_file+2>duration,sample(durs,replace = T,size=n()),start_in_file),
#                             start_in_file=ifelse(start_in_file+2>duration,sample(durs,replace = T,size=n()),start_in_file),
#                             end_in_file=start_in_file+2) %>% 
#   dplyr::select(Project,probeId,file,start_in_file,end_in_file,audit_file,SPID,grid)

hapeNeg1_streams<-hapeNeg %>% group_by(audit_file) %>% summarise(nneg=n())
hape_streams<-full_join(hapeNeg1_streams,hapePos_streams) %>% mutate(nneg=ifelse(is.na(nneg),0,nneg),npos=ifelse(is.na(npos),0,npos))

hape_streams$outpath<-paste0("\\\\NAS1\\NAS1_2Jun14\\Motherships\\HAPE_CNN_Mother_test\\",gsub(".*Figs/","",hape_streams$audit_file))
hape_streams$audit_file<-paste0("\\\\NAS1\\",gsub(".*mnt/","",hape_streams$audit_file))
hape_streams$audit_file[str_detect(hape_streams$audit_file,"NAS6")]<-gsub("\\\\NAS1","\\\\NAS2",hape_streams$audit_file[str_detect(hape_streams$audit_file,"NAS6")])
hape_streams$outdir<-dirname(hape_streams$outpath)

head(hape_streams)
hape_streams$nneg[hape_streams$nneg>750]
sum(hape_streams$nneg)
sum(hape_streams$npos)

write_csv(hape_streams,"hape_streams.csv")

library(mapdata)

data(worldHiresMapEnv)

gsub(".*Figs\\\\","",hape_streams$audit_file)

w<-map_data("worldHires") %>% filter(region=="Hawaii")


library(plotly)

p<-ggplot(w)+geom_polygon(aes(x=long,y=lat,group=group))+coord_fixed()+
  geom_point(data=as.data.frame(map_table.spdf.t),aes(x=as.numeric(Longitude),y=as.numeric(Latitude),color=as.factor(grid)))
ggplotly(p)
table(w$region=="Hawaii")