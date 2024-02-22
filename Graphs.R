setwd("~/Documents/UMBC/Meta-Analysis")

####################################### yearly means of all sites Chesapeake Bay ###################################
Perkinsus <- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Disease_data_converted.csv",)
View(Perkinsus)

Perkinsus<-na.omit(Perkinsus)
Perkinsus
Perkinsus$Prevalence<- as.numeric(Perkinsus$Prevalence)

library(lubridate)
library(dplyr)
library(ggplot2)
library(Rmisc)

Prev_means<-Perkinsus %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(Mean_Prevalence = mean(Prevalence))
View(Prev_means)

Int_means<-Perkinsus %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(Mean.Intensity = mean(Mean.Intensity))
View(Int_means)
warnings()


P_sum<- summarySE(Perkinsus, "Prevalence", groupvars= c("Year"))
View(P_sum)

I_sum <- summarySE(Perkinsus, "Mean.Intensity", groupvars = "Year")
View(I_sum)

### Plotting Annual Mean infection intensity in Chesapeake Bay ###
Intensity_plot <- ggplot(I_sum, aes(Year, Mean.Intensity)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual mean P. marinus Mean Infection Intensity in Chesapeake Bay", y = "Mean Infection intensity") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red") + geom_errorbar(aes(ymin=Mean.Intensity-se, ymax=Mean.Intensity+se),width=.2, position=position_dodge(.9))
Intensity_plot

### Plotting Annual Mean prevalence in Chesapeake Bay ###
Prev_plot <- ggplot(P_sum, aes(Year, Prevalence)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual mean P. marinus Mean Prevalence % in Chesapeake Bay", y = "Mean Prevalence %") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red") + geom_errorbar(aes(ymin=Prevalence-se, ymax=Prevalence+se),width=.2, position=position_dodge(.9))
Prev_plot

############################################ perkinsus and intensity ALL by latitude ################################

library(ggplot2)
library(dplyr)
library(tidyr)
View(Perkinsus)

Perkinsus2 <- Perkinsus[-c(3)]
Perkinsus2
View(Perkinsus2)

PM<- na.omit(Perkinsus2)
View(PM)

PM$Prevalence<- as.numeric(PM$Prevalence)
PM$Mean.Intensity<-as.numeric(PM$Mean.Intensity)

Pmeans <- Perkinsus%>%
  dplyr::group_by(Year, Lat) %>%
  dplyr::summarize(Prevalence = mean(Prevalence))
Pmeans
View(Pmeans)

Imeans <-Perkinsus %>%
  dplyr::group_by(Lat, Year) %>%
  dplyr::summarize(Mean.Intensity = mean(Mean.Intensity))
Imeans

PerkMeans<-ddply(Merged.data, .(SALINITY,Lat, Year, Month), summarise,
             Prevalence = mean(Prevalence),
             Intensity = mean(Mean.Intensity))
View(PerkMeans)

Feb<-Merged.data %>% filter(Month =='Feb')
Aug<-Merged.data %>% filter(Month =='Aug')
library(ggplot2)
warnings()

PrevLat <- ggplot(Pmeans, aes(Year, Lat, color=Prevalence)) + geom_point()+ theme_minimal()+
  scale_color_gradient2( midpoint = 50,low = "blue", mid = "yellow ", high = " red") + 
  labs(title = "Mean Annual Prevalence")+ ylab("Latitude")+geom_smooth(method= lm, se=FALSE)
PrevLat

IntLat <- ggplot(Imeans, aes(Year, Lat, color=Mean.Intensity)) + geom_point()+ theme_minimal() +
  scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 3, limits=c(0, 5)) +
  labs(title = " Mean Annual Intensity ") + ylab("Latitude") + geom_smooth(method = lm, se = FALSE)
IntLat



##########################################################################################################################################

library("plyr")
# Column wise
Means<-ddply(Merged, .(Site, Year, Month), summarise,
      mean_WTEMP = mean(WTEMP),
      mean_SALINITY = mean(SALINITY),
      mean_P = mean(Prevalence),
      mean_I = mean(Mean.Intensity))

View(Means)

PrevFeb<- Means[Means$Month == 'Feb',]
PrevMar<- Means[Means$Month == 'Mar',]
PrevJul<- Means[Means$Month == 'Jul',]
PrevSept<-Means[Means$Month == 'Sep',]
PrevDec<- Means[Means$Month == 'Dec',]


IntFeb<- Means[Means$Month == 'Feb',]
IntMar<- Means[Means$Month == 'Mar',]
IntJul<- Means[Means$Month == 'Jul',]
IntMay<-Means[Means$Month == 'May',]
IntAug<- Means[Means$Month == 'Aug',]

MeansPlotT<- ggplot(PrevSept, aes(mean_WTEMP,mean_P, color = Year))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab("Mean Prevalence ") + xlab(expression("Mean Temperature " ( degree*C)))+labs(title= 'September')+geom_smooth(se=FALSE,method=lm,col= "red")
MeansPlotT

MeansPlotS<- ggplot(Means, aes(mean_SALINITY, mean_P, color=Year))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab("Mean Prevalence ") + xlab(expression("Mean Salinity " ( degree*C)))+geom_smooth(se=FALSE, col= "red")
MeansPlotS

MeansPlotTI<-ggplot(IntAug,aes(mean_WTEMP, mean_I, color=Year))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab("Mean Intensity ") + xlab(expression("Mean Temperature " ( degree*C)))+geom_smooth(se=FALSE,col= "red")+labs(title= "August")
MeansPlotTI

MeansPlotSI<-ggplot(Means, aes(mean_SALINITY, mean_I, color=Year))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab("Yearly Mean Intensity ") + xlab(expression("Yearly Mean Salinity " ( degree*C)))+geom_smooth(se=FALSE, col= "red")
MeansPlotSI

######################################################################################
### ENVIRONMENTAL TRENDS #####
View(Means)

TemperatureTrend <- ggplot(PrevFeb, aes(mean_WTEMP, Mean_P))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab(expression("Yearly Mean Temperature " ( degree*C))) + xlab("Year")+geom_smooth(se=FALSE, method=lm,col= "red")+labs(title= 'December')
TemperatureTrend

SalinityTrend <-ggplot(Means, aes(Year,mean_SALINITY))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab("Yearly Mean Salinity (ppt)") + xlab("Year")+geom_smooth(se=FALSE, col= "red")
SalinityTrend

Means2<-ddply(Merged, .(Site, Year, Month), summarise,
             mean_WTEMP = mean(WTEMP),
             mean_SALINITY = mean(SALINITY),
             mean_P = mean(Prevalence),
             mean_I = mean(Mean.Intensity))
View(Means2)



TemperatureTrend2 <- ggplot(Means2, aes(Month,mean_WTEMP))+geom_point(aes(color=Year))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                            panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab(expression("Yearly Mean Temperature " ( degree*C))) + xlab("Month")+geom_smooth(se=FALSE, method= "lm", formula = "y~x",col= "red")+scale_x_discrete(limits = month.abb)
TemperatureTrend2

SalinityTrend2 <-ggplot(Means2, aes(Month,mean_SALINITY, color = Year))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
  ylab("Yearly Mean Salinity (ppt)") + xlab("Month")+geom_smooth(se=FALSE, method=lm,col= "red")
SalinityTrend2


##################################################################################################################

#### Effect size ####

library(readxl)
library(tidyverse)

effectsize<- read_xlsx("~/Documents/UMBC/Meta-Analysis/Intensity Effect Size.xlsx",)
View(effectsize)

Intefplot<- ggplot(effectsize, aes(fct_inorder(Month), d, color=FixedEffect))+geom_point(size=3) + theme(axis.text.x = element_text(angle=45, size = 15), axis.text.y = element_text(size=15))+
  labs(title= "B", y="",x="")+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
Intefplot

Peffectsize<- read_xlsx("~/Documents/UMBC/Meta-Analysis/Prev Effect Size.xlsx",)
View(Peffectsize)

Pefplot<- ggplot(Peffectsize, aes(fct_inorder(Month), d, color=FixedEffect))+geom_point(size=3) + theme(axis.text.x = element_text(angle=45,size = 15), axis.text.y = element_text(size=15),legend.position = "bottom",legend.text=element_text(size=15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title= "A", y="",x="")
Pefplot

library(grid)
library(gridExtra)

efplot<- grid.arrange(Pefplot, Intefplot,left= textGrob("d", gp=gpar(fontsize=20)), bottom = textGrob("Month", gp=gpar(fontsize=20)))


###### QC Environmental Data ####
View(EnvALL)
## When is sampling occurring for each site?###     

EnvALL$Parameter <- as.factor(EnvALL$Parameter)
EnvALL$Parameter <- EnvALL[EnvALL$Parameter == "WTEMP",]

E1990 <- EnvALL[EnvALL$SampleDate > "1990-01-01" & EnvALL$SampleDate < "1990-12-31",]

E2019 <- EnvALL[EnvALL$SampleDate > "2019-01-01" & EnvALL$SampleDate < "2019-12-31",]

EnvALL$SampleDate <-as.Date(EnvALL$SampleDate)

SampleDatePlot<- ggplot(E2019, aes(SampleDate, MonitoringLocation))+geom_point()+scale_x_date(date_breaks="5 day", date_labels="%Y-%m-%d")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
SampleDatePlot
View(E1990)




###### Regions most impacted by disease ###############
View(Perkinsus)
Perkinsus$oysteryear=ifelse(Perkinsus$Month== "Nov"| Perkinsus$Month=="Dec", Perkinsus$Year+1, Perkinsus$Year)
head(Perkinsus)


mean_prev_reg <- Perk2 %>%
  dplyr::group_by(Region, oysteryear, State, Lat, Long) %>%
  dplyr::summarise(mean_prevalence = mean(Prevalence))

View(mean_prev_reg)

mean_prev_reg <- filter(mean_prev_reg, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')

library(RColorBrewer)
n <- 28
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
qual_col_pals
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector

library(tidyverse)
library(dplyr)
class(mean_prev_reg$Region)

#REORDERING BY LATITUDE
mean_prev_reg$Region2<- factor(mean_prev_reg$Region, levels = c("NANTICOKE RIVER", "UPPER BAY", "CHESTER RIVER", "EASTERN BAY", "WYE RIVER", "MILES RIVER", "BROAD CREEK",
                                    "HARRIS CREEK", "CHOPTANK RIVER", "LITTLE CHOPTANK RIVER", "PATUXENT RIVER", "MIDDLE BAY", "MANOKIN RIVER",
                                    "POTOMAC RIVER", "LOWER BAY","FISHING BAY", "HONGA RIVER", "TANGIER SOUND", "HOLLAND STRAITS","POCOMOKE SOUND",
                                    "RAPPAHANNOCK RIVER","GREAT WICOMICO RIVER","CORROTOMAN RIVER","PIANKATANK RIVER","YORK RIVER","MOBJACK BAY","JAMES RIVER"))

view(mean_prev_reg)
region_p<-  ggplot(mean_prev_reg, aes(oysteryear, mean_prevalence)) + geom_point(aes(color=Region)) + 
  facet_wrap(Region~.) + theme_classic()+ geom_smooth(method= lm, se=FALSE) +scale_color_manual(values = col_vector) +
  theme(strip.text = element_text(size = 8), legend.position = "none")+ylab("Mean Prevalence") + xlab("Year")
 #+scale_color_manual(values = custom_palette)

region_p

################# GIS map Perkinsus sites  ########################################

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

Perk2 <- filter(Perk2, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')

library(RColorBrewer)
n <- 28
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
qual_col_pals
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector

OySite_area <-ggplot() + geom_sf(data = map)+theme(panel.grid.minor = element_blank(),panel.background = element_blank())+
  geom_point(data = Perk2,aes(Long, Lat, color= Region), size = 4) +theme(legend.position="none") + scale_color_manual(values = col_vector)

OySite_area

Both<-ggarrange(EnvSite_area+ rremove("ylab") + rremove("xlab"),OySite_area+ rremove("ylab") + rremove("xlab"), 
                nrow=1, labels=c("A", "B"))
Both


Regional_trends <- ggarrange(region_p, OySite_area)
Regional_trends


####### Looking at monthly trends of prev & intensity by temperature due to negative effect sizes #######




#prevalence vs temp
Jan_prev<-ggplot(Jan, aes(WTEMP, Prevalence, color = Site))+geom_point() +labs(title = element_text("January"))+theme(legend.position="none")
Apr_prev<-ggplot(Apr, aes(WTEMP, Prevalence, color = Site))+geom_point() +labs(title = element_text("April"))+theme(legend.position="none")
June_prev<-ggplot(Jun, aes(WTEMP, Prevalence, color = Site)) +geom_point()+labs(title = element_text("June"))+theme(legend.position="none")
July_prev<-ggplot(Jul, aes(WTEMP, Prevalence, color = Site)) +geom_point()+labs(title = element_text("July"))+theme(legend.position="none")
Sept_prev<-ggplot(Sept, aes(WTEMP, Prevalence, color = Site))+geom_point() +labs(title = element_text("September"))+theme(legend.position="none")
Oct_prev<-ggplot(Oct, aes(WTEMP, Prevalence, color = Site))+geom_point() +labs(title = element_text("October"))+theme(legend.position="none")
Dec_prev<-ggplot(Dec, aes(WTEMP, Prevalence, color = Site))+geom_point() +labs(title = element_text("December"))+theme(legend.position="none")

all_prev_temp<- ggarrange(Jan_prev, Apr_prev,June_prev, July_prev, Sept_prev, 
                          Oct_prev, Dec_prev)
all_prev_temp

#intensity vs temp
Jan_I<-ggplot(Jan, aes(WTEMP,Mean.Intensity, color = Site)) +geom_point()+labs(title = element_text("January"))+theme(legend.position="none")
July_I<-ggplot(Jul, aes(WTEMP,Mean.Intensity, color = Site)) +geom_point()+labs(title = element_text("July"))+theme(legend.position="none")
Sept_I<-ggplot(Sept, aes(WTEMP,Mean.Intensity, color = Site)) +geom_point()+labs(title = element_text("September"))+theme(legend.position="none")
Oct_I<-ggplot(Oct, aes(WTEMP,Mean.Intensity, color = Site))+geom_point() +labs(title = element_text("October"))+theme(legend.position="none")
Nov_I<-ggplot(Nov, aes(WTEMP,Mean.Intensity, color = Site)) +geom_point()+labs(title = element_text("November"))+theme(legend.position="none")
Dec_I<-ggplot(Dec, a2es(WTEMP,Mean.Intensity, color = Site))+geom_point() +labs(title = element_text("December"))+theme(legend.position="none")

all_int_temp<- ggarrange(Jan_I, July_I, Sept_I, Oct_I, Nov_I, Dec_I)
all_int_temp
