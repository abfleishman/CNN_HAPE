rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

library(lubridate)
library(ggplot2)
library(readr)

# Find Files and project size ---------------------------------------------

# a<-data.frame(Path=list.files(path = "D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis",full.names = T,recursive = T))
# a<-filter(a,str_detect(a$Path,"Field"),str_detect(a$Path,".csv"),!str_detect(a$Path,".jpg"),str_detect(a$Path,"deploy|Deploy")) %>% mutate(Path=as.character(Path))
# a$Project<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
# a$Project<-str_extract(a$Project,"[^/]*$")
# 
# a$year<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
# a$year<-str_extract(a$year,"[^/]*$")
# head(a,30)
# a$mas<-duplicated(a$Project)|duplicated(a$Project,fromLast = T)
# write_excel_csv(a,"deploy_list.csv")
a<-read_csv("deploy_list.csv")
head(a)
# this goes through all the log files and reads the CSV in to R
data.list1 <- lapply(a$Path, fread,sep=",")
head(data.list1,1)

# Add a project, fix location and rating, and add the csv path

for(i in 1:length(a$Project)){
  if("Sensor_Name" %in% names(data.list1[[i]])){print("Sensor_Name exists")}else{
    if(!("Sensor_Name" %in% names(data.list1[[i]]))&"Sensor"%in%names(data.list1[[i]])){ data.list1[[i]]$Sensor_Name<-data.list1[[i]]$Sensor;data.list1[[i]]$Sensor<-NULL }else{
      if(!("Sensor_Name" %in% names(data.list1[[i]]))&"Sensor_ID"%in%names(data.list1[[i]])){ data.list1[[i]]$Sensor_Name<-data.list1[[i]]$Sensor_ID }else{
        if(!("Sensor_Name" %in% names(data.list1[[i]]))&"SensorID"%in%names(data.list1[[i]])){ data.list1[[i]]$Sensor_Name<-data.list1[[i]]$SensorID }else{
          if(!("Sensor_Name" %in% names(data.list1[[i]]))&"SensorID"%in%names(data.list1[[i]])){ data.list1[[i]]$Sensor_Name<-data.list1[[i]]$`Recording Unit` }else{
            stop("check your column names and make sure Sensor_Name exists")}}}}}
  if(!("Easting" %in% names(data.list1[[i]]))){ data.list1[[i]]$Easting<-data.list1[[i]]$x_proj }
  if(!("Easting" %in% names(data.list1[[i]]))){ data.list1[[i]]$Easting<-data.list1[[i]]$x }
  if(!("Easting" %in% names(data.list1[[i]]))){ data.list1[[i]]$Easting<-data.list1[[i]]$Westing }
  if(!("Latitude" %in% names(data.list1[[i]]))){ data.list1[[i]]$Latitude<-data.list1[[i]]$`Lat(DecDeg)` }
  if(!("Longitude" %in% names(data.list1[[i]]))){ data.list1[[i]]$Longitude<-data.list1[[i]]$`Long(DecDeg)` }
  
  
  data.list1[[i]]<-data.list1[[i]] %>% select(one_of("SPID","Sensor_Name","Deployment_Date","Deployment_Time","Retrieval_Date","Retrieval_Time","Easting","Northing","Latitude","Longitude",'Island',"UTMs","Site_Name","Location","Location_Name")) %>% mutate_all(.funs = as.character)
  data.list1[[i]]$Project<-a$Project[i]
  data.list1[[i]]$Project<-a$Project[i] 
  data.list1[[i]]$csv_path<-a$Path[i]

}


Names<-NULL
for(i in 1:length(a$Project)){
  Names<-c(Names,names(data.list1[[i]]))
}
data.frame(table(Names))
SPdata<-bind_rows(data.list1)

# write_csv(SPdata,"SPData.csv") 
SPdata<-read_csv("SPData.csv")
# write_csv(data.frame(unique(SPdata %>% filter(is.na(Latitude),!is.na(Northing)) %>% select(Island) )),"espg.csv")
ESPG<-read_csv("espg.csv") %>% mutate(UTM=str_replace_all(UTM,'\\"',""))
                                              
SPdata_noLat<-SPdata %>% left_join(ESPG) %>% filter(is.na(Latitude),!is.na(Northing))
# Transformed data
library(proj4)
for(i in 1:nrow(SPdata_noLat)){
  if(is.na(SPdata_noLat$Northing[i])) next
  dat<-data.frame(x=SPdata_noLat$Northing[i],y=SPdata_noLat$Easting[i])
  # coordinates(dat)<-cbind(dat$x,dat$y)
  # crs(dat)<-CRS("+init=epsg:4326")
  pj <- project(data.frame(x=SPdata_noLat$Northing[i],y=SPdata_noLat$Easting[i]), proj = "+proj=utm +zone=4 +north +datum=WGS84 " , inverse=TRUE)
  SPdata_noLat$Latitude[i] <- pj$y
  SPdata_noLat$Longitude[i] <- pj$x
}
SPdata_f<-filter(SPdata, !is.na(Latitude),is.na(Northing)) %>% bind_rows(SPdata_noLat) %>%  select(Project,SPID,Sensor_Name,Latitude,Longitude, Deployment_Date, Deployment_Time, Retrieval_Date ,Retrieval_Time,csv_path )
head(SPdata_f)
write_csv(SPdata_f,"SPID_Table.csv")
w<-map_data("world")
install.packages("plotly")
library(plotly)

p<-ggplot(w)+geom_polygon(aes(x=long,y=lat,group=group))+coord_fixed()+geom_point(data=SPdata_f %>% filter(Longitude<180),aes(x=as.numeric(Longitude),y=as.numeric(Latitude)),color="red")
ggplotly(p)
