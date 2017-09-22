rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

library(lubridate)
library(ggplot2)
library(readr)

# Find Files  ---------------------------------------------

# a<-data.frame(Path=list.files(path = "D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis",pattern = ".csv",full.names = T,recursive = T))
# b<-file.info(a$Path)
# b<-mutate(b,Path=rownames(b))
# b1<-b
# a<-b%>% mutate(Path=as.character(Path)) %>% filter(!str_detect(Path,"Report/Tables|Hits|Presentation|CallsPerMin|metric|Metrics|hmm|Test|log2csv|NESH_Sep_2012|Rates|Teststrip|test_strip|Test_Strip|SUmmary|playback|Log_Files|log_files|Log_files|clip|Clip|R_output|R_Output|Results|Mean|mean|figures|Total|total|power|NewNames4|sun|Sun|rise|Rise|Twilight|twilight|Illu|illu|Moon|moon|TEMP|temp|Temp"),
#                                                    str_detect(Path,"SPID|SPid|SPId|location|Location|survey|Survey|field|Field|site|Site|Deploy|deploy"),
#                                                    size<500000,size>1)
# a$Project<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
# a$Project<-str_extract(a$Project,"[^/]*$")
# 
# a$year<-str_extract(a$Path,"[^/]*/[^/]*/[^/]*/[^/]*/[^/]*/[^/]*")
# a$year<-str_extract(a$year,"[^/]*$")
# head(a,30)
# 
# a$mas<-duplicated(a$Project)|duplicated(a$Project,fromLast = T)
# a1<-read_delim("deploy_list_raw_AF.tsv",delim = "\t") %>% mutate(year=as.character(year))
# a<-left_join(a,a1)
# table(a1$keep)
# table(a$keep)
# write_excel_csv(a,"deploy_list_raw.csv")
# a<-read_delim("deploy_list_raw_AF.tsv",delim = "\t")
a<-read_delim("deploy_list_raw.tsv",delim = "\t")
a<-a %>% filter(keep==T)
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
  
  if(!("Easting" %in% names(data.list1[[i]]))&("x_proj" %in% names(data.list1[[i]]))){ data.list1[[i]]$Easting<-data.list1[[i]]$x_proj }
  if(!("Easting" %in% names(data.list1[[i]]))&("y_proj" %in% names(data.list1[[i]]))){ data.list1[[i]]$Easting<-data.list1[[i]]$x_proj }
  if(!("Northing" %in% names(data.list1[[i]]))&("Westing" %in% names(data.list1[[i]]))){ data.list1[[i]]$Northing<-data.list1[[i]]$y_proj }
  if(!("Latitude" %in% names(data.list1[[i]]))&("Long(DecDeg)"  %in% names(data.list1[[i]]))){ data.list1[[i]]$Latitude<-data.list1[[i]]$`Lat(DecDeg)` }
  if(!("Longitude" %in% names(data.list1[[i]]))&("Long(DecDeg)"  %in% names(data.list1[[i]]))){ data.list1[[i]]$Longitude<-data.list1[[i]]$`Long(DecDeg)` }
  if(!("Longitude" %in% names(data.list1[[i]]))&("Start_Longitude" %in% names(data.list1[[i]]))){ data.list1[[i]]$Longitude<-data.list1[[i]]$Start_Longitude }
  if(!("Latitude" %in% names(data.list1[[i]]))&("Start_Latitude" %in% names(data.list1[[i]]))){ data.list1[[i]]$Latitude<-data.list1[[i]]$Start_Latitude }
  
  
  data.list1[[i]]<-data.list1[[i]] %>% select(one_of("SPID","Sensor_Name","Deployment_Date","Deployment_Time","Retrieval_Date",
                                                     "Retrieval_Time","Easting","Northing","Latitude","Longitude",'Island')) %>% mutate_all(.funs = as.character)
  data.list1[[i]]$Project<-a$Project[i]
  data.list1[[i]]$Project<-a$Project[i] 
  data.list1[[i]]$csv_path<-a$Path[i]

}


Names<-NULL
for(i in 1:length(a$Project)){
  Names<-bind_rows(Names,data.frame(names=c(names(data.list1[[i]])),i=i))
  print(i)
  print(data.list1[[i]]$Easting)
  }
Names %>% dplyr::filter(names=="LOCATION")
unique(Names$names)

  
data.frame(table(Names$names))
SPdata<-bind_rows(data.list1)

# SPdata$Island
# write_csv(SPdata,"SPData.csv") 
# SPdata<-read_csv("SPData.csv")
# write_csv(data.frame(unique(SPdata %>% filter(is.na(Latitude),!is.na(Northing)) %>% select(Island) )),"espg.csv")
ESPG<-read_csv("espg.csv") %>% mutate(UTM=str_replace_all(UTM,'\\"',""))
                                              
SPdata_noLat_ULP2012<-SPdata  %>% filter(is.na(Latitude),!is.na(Northing),Project=="KESRP_ULP_2012")%>% mutate(Island="Kauai") %>% left_join(ESPG)
SPdata_noLat<-SPdata %>% filter(is.na(Latitude),!is.na(Northing),Project!="KESRP_ULP_2012")%>% left_join(ESPG)
# names(SPdata_noLat_ULP2012)[7:8]<-c( "Northing", "Easting")
SPdata_noLat<-SPdata_noLat %>% bind_rows(SPdata_noLat_ULP2012)

# Transformed data
library(proj4)
for(i in 1:nrow(SPdata_noLat)){
  if(is.na(SPdata_noLat$Northing[i])|is.na(SPdata_noLat$UTM[i])) next
  dat<-data.frame(x=as.numeric(SPdata_noLat$Easting[i]),y=as.numeric(SPdata_noLat$Northing[i]))
  # coordinates(dat)<-cbind(dat$x,dat$y)
  # crs(dat)<-CRS("+init=epsg:4326")
  pj <- project(dat, proj =SPdata_noLat$UTM[i], inverse=TRUE)
  SPdata_noLat$Latitude[i] <- pj$y
  SPdata_noLat$Longitude[i] <- pj$x
}
head(SPdata)
SPdata_f<-filter(SPdata, is.na(Northing)|(!is.na(Latitude)&!is.na(Northing))) %>% bind_rows(SPdata_noLat) %>%  select(Project,SPID,Sensor_Name,Latitude,Longitude, Deployment_Date, Deployment_Time, Retrieval_Date ,Retrieval_Time,csv_path )

head(SPdata_f)
write_csv(SPdata_f,"SPID_Table.csv")
w<-map_data("world")
library(plotly)
hist(as.numeric(SPdata$Easting))
unique(as.numeric(SPdata$Northing))

p<-ggplot(w)+geom_polygon(aes(x=long,y=lat,group=group))+coord_fixed()+geom_point(data=SPdata_f %>% filter(Longitude<180),aes(x=as.numeric(Longitude),y=as.numeric(Latitude),color=Project))
ggplotly(p)
