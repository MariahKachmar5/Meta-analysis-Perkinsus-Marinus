
### Distance Matrix for Perkinsus marinus disease bar sites and environmental 
### monitoring sites in the Chesapeake Bay

#Set working directory
setwd("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/")

require(sp)
require(tidyr)
library(readxl)

options(repr.plot.width=20, repr.plot.height=15)

env =read_excel("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/EnvironmentalData_MD&VAupdated.xlsx")
perk=read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Disease_data_converted.csv", header=FALSE)

View(env)
View(perk)
              
#pull out env sites
env_sites <-env[,c(1,8,9)]
              
env_sites=unique(env_sites)
              
              
#pull out longlat
env_longlat=as.matrix(sapply(env_sites[,c(2,3)], as.numeric))
              
#env_longlat=na.omit(env_longlat)
              
              
#pull out perksites
perk_sites=perk[,5:7]
              
              
colnames(perk_sites)=c("Site", "Latitude", "Longitude")
              
              
perk_sites$Site=gsub(" ", "", perk_sites$Site) 
 


perk_sites=unique(perk_sites)


#perk_sites=perk_sites[-which(perk_sites$Latitude == ""), ]


#take out NAs
perk_sites=drop_na(perk_sites, Latitude)


#pull out longlat
perk_longlat=as.matrix(sapply(perk_sites[,c(2,3)], as.numeric))

perk_longlat<-na.omit(perk_longlat)

#make distance matrix (euclidean)
distsall=spDists(perk_longlat, env_longlat)

View(distsall)

colnames(distsall)=env_sites$MonitoringLocation

perk_sites = perk_sites[-c(1),]
perk_sites

rownames(distsall)=perk_sites$Site

perk_sites$Site

(distsall)

#make dataframe of minimum distances and sites
mindists=as.data.frame(rownames(distsall))
mindists$min=apply(distsall, 1, min)
mindists$envsites=apply(distsall, 1, function(x) colnames(distsall)[which.min(x)])
colnames(mindists)=c("perksite", "distance", "envsites")

mindists

write.csv(mindists, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Closest_PerkEnv.csv")

#Loading required package: ggplot2
require(ggplot2)



ggplot(mindists, aes(x=perksite, y=min))+
  geom_point(size=4)+
  theme_bw()+
  theme(legend.position="right", panel.border = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank(), axis.line=element_line(color="black"), axis.text=element_text(color="black",, angle=90),
        text = element_text(size=15))
