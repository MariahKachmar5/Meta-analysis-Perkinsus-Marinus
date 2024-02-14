setwd("~/Documents/UMBC/Meta-Analysis")

### GIS map Perkinsus sites MD ###

options(repr.plot.width=20, repr.plot.height=15)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(dplyr)
library(rgdal)
library(geosphere)
library(plotrix)
library(ggrepel)
library(sf)
library(ggpubr)
library(grid)

map_new <- st_read("~/Documents/UMBC/Meta-Analysis/ChesapeakeBay/Chesapeake_Bay_Shoreline_High_Resolution/", "Chesapeake_Bay_Shoreline_High_Resolution")
map_new

options(sf_max.plot=1)
plot(map_new, axes=TRUE)

map<-fortify(map_new)

View(Merged.data)

Env2<-read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VAupdated.xlsx")
View(Env2)

Env2$Latitude<-as.numeric(Env2$Latitude)
Env2$Longitude<- as.numeric(Env2$Longitude)

EnvSite_area<-ggplot() + geom_sf(data = map)+theme(panel.grid.minor = element_blank(),panel.background = element_blank())+geom_point(data = Env2,aes(Longitude, Latitude, color = MonitoringLocation ))+theme(legend.position="none")
EnvSite_area


OySite_area <-ggplot() + geom_sf(data = map)+theme(panel.grid.minor = element_blank(),panel.background = element_blank())+
  geom_text(data = Perk2,aes(Long, Lat, color= Region, label = Region), size = 2)#+theme(legend.position="none")

OySite_area

Both<-ggarrange(EnvSite_area+ rremove("ylab") + rremove("xlab"),OySite_area+ rremove("ylab") + rremove("xlab"), 
                nrow=1, labels=c("A", "B"))
Both


