setwd("~/Documents/UMBC/Github/Meta-analysis-Perkinsus-Marinus/code")

####################################### yearly means of all sites Chesapeake Bay ###################################
Perkinsus <- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/outputs/Perkinsus_data_converted.csv",)
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




### Yearly Mean Temp and Salinity ###

MeanAnnual<- Merged.data %>%
  dplyr::group_by(Region, Year, Site, MonitoringLocation, Prev_ep, Mean.Intensity) %>%
  dplyr::summarize(Mean_Temp = mean(WTEMP), Mean_Sal = mean(SALINITY))
View(MeanAnnual)

View(MasterENV_filtered)

T_sum<- summarySE(Merged.data, "WTEMP", groupvars= c("Year", "Region"))
View(T_sum)

S_sum <- summarySE(Merged.data, "SALINITY", groupvars = c("Year", "Region"))
View(S_sum)

#AnnualTemp<- MasterENV_filtered %>%
#  dplyr::group_by(Year) %>%
#  dplyr::summarize(Mean_Temp = mean(WTEMP))
#View(AnnualTemp)

AnnualTemp_plot <- ggplot(T_sum, aes(Year, WTEMP)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual Mean Temperature in Chesapeake Bay", y = "Mean Temperature") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red") + geom_errorbar(aes(ymin=WTEMP-se, ymax=WTEMP+se),width=.2, position=position_dodge(.9))
AnnualTemp_plot

AnnualSAL_plot <- ggplot(S_sum, aes(Year, SALINITY)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual Mean Salinity in Chesapeake Bay", y = "Mean Salinity") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red") + geom_errorbar(aes(ymin=SALINITY-se, ymax=SALINITY+se),width=.2, position=position_dodge(.9))
AnnualSAL_plot

##################################################################################################################

#### Effect size ####

library(readxl)
library(tidyverse)

effectsize<- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/data_raw/MonthlyEffectSizes.csv",)


P_effect<- effectsize %>% filter(variable == "Prevalence")
I_effect<- effectsize %>% filter(variable == "Intensity")

Intefplot<- ggplot(I_effect, aes(fct_inorder(month), d, color=fixed_effect))+geom_point(size=3) + theme(axis.text.x = element_text( size = 15), axis.text.y = element_text(size=15))+
  labs(title= "B", y="",x="")+theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black")) 
Intefplot


Pefplot<- ggplot(P_effect, aes(fct_inorder(month), d, color=fixed_effect))+geom_point(size=3) + theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=15),legend.position = "bottom",legend.text=element_text(size=15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title= "A", y="",x="")
Pefplot

library(grid)
library(gridExtra)

efplot<- grid.arrange(Pefplot, Intefplot,left= textGrob("d", gp=gpar(fontsize=20)), bottom = textGrob("month", gp=gpar(fontsize=20)))

##################################################################################
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


#################################################################################

###### Regions most impacted by disease ###############
View(Perkinsus)
Perkinsus$oysteryear=ifelse(Perkinsus$Month== "Nov"| Perkinsus$Month=="Dec", Perkinsus$Year+1, Perkinsus$Year)
head(Perkinsus)

#Prevalence 
Merged.data$Prevalence<- as.numeric(Merged.data$Prevalence)
View(Merged.data)

mean_prev_reg <- Merged.data %>%
  dplyr::group_by(Region, oysteryear, State, Lat, Long) %>%
  dplyr::summarise(mean_prevalence = mean(Prevalence))

View(mean_prev_reg)

mean_prev_reg <- filter(mean_prev_reg, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')

#Mean Intensity
mean_int_reg <- Merged.data %>%
  dplyr::group_by(Region, oysteryear, State, Lat, Long) %>%
  dplyr::summarise(mean_intensity = mean(Mean.Intensity))

View(mean_int_reg)

mean_int_reg <- filter(mean_int_reg, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')

library(RColorBrewer)
n <- 40
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
qual_col_pals
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector



colors <- c("#FF0000", "#0033FF", "#FF6600", "#9900FF", "#00FFFF", "#FF00FF", "#FFFF00", "#993333", "#999999",
                    "#006600", "#FF9999", "#330099", "#00CCFF","#99FF33", "#FFCCFF", "#000000", "#CC9966", "#336666",
                    "#FFCC66", "#003300", "#0099FF", "#FF3399", "#FF9933", "#0066FF", "#6600CC", "#33FFCC", "#CC9999", "#FFFFCC")
                         

library(tidyverse)
library(dplyr)
class(mean_prev_reg$Region)

#REORDERING BY LATITUDE
mean_prev_reg$Region2<- factor(mean_prev_reg$Region, levels = c("NANTICOKE RIVER", "UPPER BAY", "CHESTER RIVER", "EASTERN BAY", "WYE RIVER", "MILES RIVER", "BROAD CREEK",
                                    "HARRIS CREEK", "CHOPTANK RIVER", "LITTLE CHOPTANK RIVER", "ST. MARY'S RIVER", "PATUXENT RIVER", "MIDDLE BAY", "MANOKIN RIVER",
                                    "POTOMAC RIVER", "LOWER BAY","FISHING BAY", "HONGA RIVER", "TANGIER SOUND", "HOLLAND STRAITS","POCOMOKE SOUND",
                                    "RAPPAHANNOCK RIVER","GREAT WICOMICO RIVER","CORROTOMAN RIVER","PIANKATANK RIVER","YORK RIVER","MOBJACK BAY","JAMES RIVER"))

mean_int_reg$Region2 <- factor(mean_int_reg$Region, levels = c("NANTICOKE RIVER", "UPPER BAY", "CHESTER RIVER", "EASTERN BAY", "WYE RIVER", "MILES RIVER", "BROAD CREEK",
                                                                "HARRIS CREEK", "CHOPTANK RIVER", "LITTLE CHOPTANK RIVER", "ST. MARY'S RIVER", "PATUXENT RIVER", "MIDDLE BAY", "MANOKIN RIVER",
                                                                "POTOMAC RIVER", "LOWER BAY","FISHING BAY", "HONGA RIVER", "TANGIER SOUND", "HOLLAND STRAITS","POCOMOKE SOUND",
                                                                "RAPPAHANNOCK RIVER","GREAT WICOMICO RIVER","CORROTOMAN RIVER","PIANKATANK RIVER","YORK RIVER","MOBJACK BAY","JAMES RIVER"))


View(mean_prev_reg)
region_p<-  ggplot(mean_prev_reg, aes(oysteryear, mean_prevalence)) + geom_point(aes(color=Region2)) + 
  facet_wrap(Region2~.) + theme_classic()+ geom_smooth(method= lm, se=FALSE) +scale_color_manual(values = colors) +
  theme(strip.text = element_text(size = 15), axis.text=element_text(size=15),axis.title=element_text(size=15), legend.position = "none")+ylab("Mean Prevalence") + xlab("Year")
 #+scale_color_manual(values = custom_palette)

region_p

mean_int_reg<-na.omit(mean_int_reg)
region_i<-  ggplot(mean_int_reg, aes(oysteryear, mean_intensity)) + geom_point(aes(color=Region2)) + 
  facet_wrap(Region2~.) + theme_classic()+ geom_smooth(method= lm, se=FALSE) +scale_color_manual(values = colors) +
  theme(strip.text = element_text(size = 15), axis.text=element_text(size=15),axis.title=element_text(size=15), legend.position = "none")+ylab("Mean Intensity") + xlab("Year")
#+scale_color_manual(values = custom_palette)

region_i

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

Env2<-read_excel("~/Documents/UMBC/Meta-Analysis/environmental_data_all.xlsx")
View(Env2)

Env2$Latitude<-as.numeric(Env2$Latitude)
Env2$Longitude<- as.numeric(Env2$Longitude)

EnvSite_area<-ggplot() + geom_sf(data = map)+theme(panel.grid.minor = element_blank(),panel.background = element_blank())+geom_point(data = Env2,aes(Longitude, Latitude, color = MonitoringLocation ))+theme(legend.position="none")
EnvSite_area

Perk2 <- filter(Perk2, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')


OySite_area <-ggplot() + geom_sf(data = map)+theme(panel.grid.minor = element_blank(),panel.background = element_blank())+
  geom_point(data = mean_prev_reg,aes(Long, Lat, color= Region2), size = 3) +theme(legend.position="none") + scale_color_manual(values = colors)

OySite_area

Both<-ggarrange(EnvSite_area+ rremove("ylab") + rremove("xlab"),OySite_area+ rremove("ylab") + rremove("xlab"), 
                nrow=1, labels=c("A", "B"))
Both


Regional_trends <- ggarrange(region_p, OySite_area)
Regional_trends

View(Merged.data)

####### Looking at monthly trends of prev & intensity by temperature due to negative effect sizes #######

Jan <- Merged.data[Merged.data$Month == "Jan",]
Feb<- Merged.data[Merged.data$Month == "Feb",]
Mar<- Merged.data[Merged.data$Month == "Mar",]
Apr<- Merged.data[Merged.data$Month == "Apr",]
May<- Merged.data[Merged.data$Month == "May",]
Jun<- Merged.data[Merged.data$Month == "Jun",]
Jul<- Merged.data[Merged.data$Month == "Jul",]
Aug<- Merged.data[Merged.data$Month == "Aug",]
Sept<- Merged.data[Merged.data$Month == "Sep",]
Oct<- Merged.data[Merged.data$Month == "Oct",]
Nov<- Merged.data[Merged.data$Month == "Nov",]
Dec<- Merged.data[Merged.data$Month == "Dec",]

Jan$Prevalence<- as.numeric(Jan$Prevalence)
Feb$Prevalence<- as.numeric(Feb$Prevalence)
Mar$Prevalence<- as.numeric(Mar$Prevalence)
Apr$Prevalence<- as.numeric(Apr$Prevalence)
May$Prevalence<- as.numeric(May$Prevalence)
Jun$Prevalence<- as.numeric(Jun$Prevalence)
Jul$Prevalence<- as.numeric(Jul$Prevalence)
Aug$Prevalence<- as.numeric(Aug$Prevalence)
Sept$Prevalence<- as.numeric(Sept$Prevalence)
Oct$Prevalence<- as.numeric(Oct$Prevalence)
Nov$Prevalence<- as.numeric(Nov$Prevalence)
Dec$Prevalence<- as.numeric(Dec$Prevalence)

Jan$Mean.Intensity<- as.numeric(Jan$Mean.Intensity)
Feb$Mean.Intensity<- as.numeric(Feb$Mean.Intensity)
Mar$Mean.Intensity<- as.numeric(Mar$Mean.Intensity)
Apr$Mean.Intensity<- as.numeric(Apr$Mean.Intensity)
May$Mean.Intensity<- as.numeric(May$Mean.Intensity)
Jun$Mean.Intensity<- as.numeric(Jun$Mean.Intensity)
Jul$Mean.Intensity<- as.numeric(Jul$Mean.Intensity)
Aug$Mean.Intensity<- as.numeric(Aug$Mean.Intensity)
Sept$Mean.Intensity<- as.numeric(Sept$Mean.Intensity)
Oct$Mean.Intensity<- as.numeric(Oct$Mean.Intensity)
Nov$Mean.Intensity<- as.numeric(Nov$Mean.Intensity)
Dec$Mean.Intensity<- as.numeric(Dec$Mean.Intensity)


## Prevalence ##
Jan_mean<- Jan %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Feb_mean<- Feb %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Mar_mean<- Mar %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Apr_mean<- Apr %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
May_mean<- May %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
June_mean<- Jun %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
July_mean<- Jul %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Aug_mean<- Aug %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Sept_mean<- Sept %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Oct_mean<- Oct %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Dec_mean<- Dec %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
Nov_mean<- Nov %>%
  group_by(oysteryear, WTEMP)%>%
  summarize(mean_prevalence = mean(Prevalence))
#prevalence vs temp
Jan_prev<-ggplot(Jan_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("January"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Feb_prev<-ggplot(Feb_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("February"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Mar_prev<-ggplot(Mar_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("March"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Apr_prev<-ggplot(Apr_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("April"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
May_prev<-ggplot(May_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("May"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
June_prev<-ggplot(June_mean, aes(WTEMP, mean_prevalence)) +geom_point()+labs(title = element_text("June"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
July_prev<-ggplot(July_mean, aes(WTEMP, mean_prevalence)) +geom_point()+labs(title = element_text("July"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Sept_prev<-ggplot(Sept_mean, aes(WTEMP, mean_prevalence ))+geom_point() +labs(title = element_text("September"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Oct_prev<-ggplot(Oct_mean, aes(WTEMP, mean_prevalence ))+geom_point() +labs(title = element_text("October"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Dec_prev<-ggplot(Dec_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("December"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Aug_prev<-ggplot(Aug_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("August"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Nov_prev<-ggplot(Nov_mean, aes(WTEMP, mean_prevalence))+geom_point() +labs(title = element_text("November"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)

all_prev_temp<- ggarrange(Jan_prev, Feb_prev, Mar_prev,Apr_prev,
                          May_prev,June_prev, July_prev, 
                          Aug_prev,Sept_prev, 
                          Oct_prev, Nov_prev,Dec_prev)
all_prev_temp

View(Aug_mean)
View(Aug)

## Intensity ##
Jan_mean2<- Jan %>%
  group_by(oysteryear, WTEMP) %>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Feb_mean2<- Feb %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Mar_mean2<- Mar %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Apr_mean2<- Apr %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
May_mean2<- May %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
June_mean2<- Jun %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
July_mean2<- Jul %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Aug_mean2<- Aug %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Sept_mean2<- Sept %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Oct_mean2<- Oct %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Dec_mean2<- Dec %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
Nov_mean2<- Nov %>%
  group_by(oysteryear, WTEMP)%>%
  dplyr::summarize(mean_I = mean(Mean.Intensity))
#prevalence vs temp
Jan_I<-ggplot(Jan_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("January"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Feb_I<-ggplot(Feb_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("February"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Mar_I<-ggplot(Mar_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("March"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Apr_I<-ggplot(Apr_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("April"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
May_I<-ggplot(May_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("May"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
June_I<-ggplot(June_mean2, aes(WTEMP, mean_I)) +geom_point()+labs(title = element_text("June"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
July_I<-ggplot(July_mean2, aes(WTEMP, mean_I)) +geom_point()+labs(title = element_text("July"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Sept_I<-ggplot(Sept_mean2, aes(WTEMP, mean_I ))+geom_point() +labs(title = element_text("September"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Oct_I<-ggplot(Oct_mean2, aes(WTEMP, mean_I ))+geom_point() +labs(title = element_text("October"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Dec_I<-ggplot(Dec_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("December"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Aug_I<-ggplot(Aug_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("August"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)
Nov_I<-ggplot(Nov_mean2, aes(WTEMP, mean_I))+geom_point() +labs(title = element_text("November"))+theme(legend.position="none")+geom_smooth(method=lm, se=FALSE)

all_int_temp<- ggarrange(Jan_I, Feb_I, Mar_prev,Apr_I,
                          May_I,June_I, July_I, 
                          Aug_I,Sept_I, 
                          Oct_I, Nov_I,Dec_I)
all_int_temp


############### How far away are the environmental sites from the disease bar sites? ##############


Locations<- Merged.data %>%
  subset(select= c(MonitoringLocation, Region, Site, State, Lat, Long, Lat_Env, Long_Env))
View(Locations)  

filtered_locations<- Locations %>% distinct(MonitoringLocation, Region, Site, State, Lat, Long, Lat_Env, Long_Env, .keep_all = TRUE)
filtered_locations<-na.omit(filtered_locations)
class(filtered_locations$Site)
filtered_locations$Site <- as.factor(filtered_locations$Site)

filtered_locations$Site

Locations_map <-ggplot() + geom_sf(data = map)+ theme(panel.grid.minor = element_blank(),panel.background = element_blank())

Locations_map +geom_point(data = filtered_locations,aes(Long, Lat), color="red", size = 4) + 
  geom_point(data=filtered_locations, aes(Long_Env, Lat_Env), color="black",size = 4) +
  theme(legend.position="none") #+ scale_color_discrete(name = "Site")

library(mapview)
library(leaflet)
filtered_locations$Long <- as.numeric(filtered_locations$Long)
filtered_locations$Lat <- as.numeric(filtered_locations$Lat)
filtered_locations$Long_Env <- as.numeric(filtered_locations$Long_Env)

filtered_locations$Lat_Env <- as.numeric(filtered_locations$Lat_Env)

envlocations<- filtered_locations %>%
  subset(select=c(MonitoringLocation, Region, State, Lat_Env, Long_Env))

mapview(filtered_locations, xcol= "Long", ycol = "Lat", crs=4326, zcol="Site" ,grid=FALSE) + mapview(envlocations, xcol="Long_Env", ycol="Lat_Env",col.regions= "red", grid=FALSE)


disease<-filtered_locations %>%
  subset(select=c(Site, Region, State, Lat, Long))

disease$type<- "disease"
head(disease)

envlocations$type<- "environment"
head(envlocations)
envlocations<-envlocations %>%
  dplyr::rename(Lat= Lat_Env, Long= Long_Env, Site = MonitoringLocation)

monitoringlocations<- rbind(disease, envlocations)
monitoringlocations
library(dplyr)
MD <- monitoringlocations %>%
  filter(State == "MD")

VA<- monitoringlocations %>%
  filter(State == "VA")




Locations_map <-ggplot() + geom_sf(data = map)+ theme(panel.grid.minor = element_blank(),panel.background = element_blank()) + 
  geom_point(data = filtered_locations ,aes(Long, Lat), color="red", size = 3) + 
  geom_point(data=monitoringlocations, aes(Long, Lat, color=type),size = 3)  +
  theme(legend.position= "") + labs(title =element_text("Locations of disease & environmental monitoring"))
Locations_map



map<-ggplot() + geom_point(data = MD,aes(Long, Lat, color = Site, shape = type), size = 5)+theme(legend.position="bottom") #+ scale_color_manual(values =rainbow(47))
map



####### Deep diving into James River ##############

# James river is weird. Both prevalence and intensity is decreasing but both temperature and salinity
# have a significant effect. Salinity and year are also positively correlated. 

# Lets look at the james river sites individually 


View(James)

James<- filter(James, Site != 'LONG ROCK')

TemperatureJames <- ggplot(James, aes(Year, WTEMP), fill= Site) + 
  geom_point()+geom_smooth(method=lm, se=FALSE)+ facet_wrap( ~Site, nrow=5) +theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.line.x.bottom  = element_line(color = "black"),
    axis.line.y = element_line(color = "black"), panel.spacing = unit(1, "lines")
  ) +ylab("Temperature (Celsisus)")
TemperatureJames


#Salinity mean 

Salinity_James <- ggplot(James, aes(Year, SALINITY), fill= Site) + geom_point()+geom_smooth(method=lm, se=FALSE)
Salinity_James

SalinityJames <- ggplot(James, aes(Year, SALINITY), fill= Site) + geom_point()+geom_smooth(method=lm, se=FALSE)+ facet_wrap( ~Site, nrow=5) +theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.line.x.bottom  = element_line(color = "black"),
    axis.line.y = element_line(color = "black"), panel.spacing = unit(1, "lines")
  ) +ylab("Salinity")
SalinityJames

#Salinity is not increasing at all sites 

#what is happening with perkinsus at each site?
PrevJames <- ggplot(James, aes(Year, Prev_ep), fill= Site) + geom_point()+geom_smooth(method=lm, se=FALSE)+ facet_wrap( ~Site, nrow=5) +theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.line.x.bottom  = element_line(color = "black"),
    axis.line.y = element_line(color = "black"), panel.spacing = unit(1, "lines")
  ) +ylab("Prevalence")
PrevJames

IntJames <- ggplot(James, aes(Year, Mean.Intensity), fill= Site) + geom_point()+geom_smooth(method=lm, se=FALSE)+ facet_wrap( ~Site, nrow=5) +theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.line.x.bottom  = element_line(color = "black"),
    axis.line.y = element_line(color = "black"), panel.spacing = unit(1, "lines")
  ) +ylab("Intensity")
IntJames


View(Feb)
View(PocomokeS)
View(York)

Feb_filter<- Feb[Feb$Region == "MANOKIN RIVER" | Feb$Region =="YORK RIVER"
| Feb$Region =="POCOMOKE SOUND",]
Feb_filter

Temp_Feb <- ggplot(Feb_filter, aes(Year, WTEMP)) + geom_point()+geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~Region)
Temp_Feb

#What is going on with the POCOMOKE SOUND ?

May_filter<- May[May$Region == "MANOKIN RIVER" | May$Region =="YORK RIVER"
                 | May$Region =="POCOMOKE SOUND",]
May_filter

Sal_May <- ggplot(May_filter, aes(Year, SALINITY)) + geom_point()+geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~Region)
Sal_May


June_filter<- Jun[Jun$Region == "MANOKIN RIVER" | Jun$Region =="YORK RIVER"
                 | Jun$Region =="POCOMOKE SOUND",]
June_filter

Sal_June <- ggplot(June_filter, aes(Year, SALINITY)) + geom_point()+geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~Region)
Sal_June


Jul_filter<- Jul[Jul$Region == "MANOKIN RIVER" | Jul$Region =="YORK RIVER"
                  | Jul$Region =="POCOMOKE SOUND",]
Jul_filter

Sal_Jul <- ggplot(Jul_filter, aes(Year, SALINITY)) + geom_point()+geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~Region)
Sal_Jul


Aug_filter<- Aug[Aug$Region == "MANOKIN RIVER" | Aug$Region =="YORK RIVER"
                 | Aug$Region =="POCOMOKE SOUND",]
Aug_filter

Sal_Aug <- ggplot(Aug_filter, aes(Year, SALINITY)) + geom_point()+geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~Region) 
Sal_Aug







































































































































































































































































































































































































































































































































































































































































































































































































































































































































