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

##################################################################################
######### SEASONS ###############

#### TEMPERATURE ######

Merged<-read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/MergedData2.csv",)
View(Merged)

Merged<-na.omit(Merged.data)
View(Merged)
library(ggplot2)

View(Merged.data)

Springtemp <- Merged[Merged$Month == "Mar" | Merged$Month== "Apr"|
                            Merged$Month == "May",]
View(Springtemp)

Summertemp  <- Merged[Merged$Month == "Jun" | Merged$Month== "Jul"|
                             Merged$Month == "Aug",]
View(Summertemp)

Falltemp <-Merged[Merged$Month == "Sept" | Merged$Month== "Oct"|
                         Merged$Month == "Nov",]

Wintertemp<-Merged[Merged$Month == "Dec" | Merged$Month== "Jan"|
                          Merged$Month == "Feb",]
View(Wintertemp)

#### Spring graph - annual means by and site ####
library(dplyr)
STmeans <-Springtemp %>%
  group_by(Lat, oysterear, Site) %>%
  summarize(Temperature = mean(WTEMP))
View(STmeans)

Spring_means_plot <- ggplot(STmeans, aes(Year, Temperature, color=Site)) + geom_smooth(se = FALSE, method=lm) +labs(title= "Spring", x="", y="")+ theme(legend.position = "none")
Spring_means_plot

library(ggplot2)

##### Fall graph - annual temp means by site ######

FTmeans <-Falltemp %>%
  group_by(Lat, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(FTmeans)

Fall_means_plot <- ggplot(FTmeans, aes(Year, Temperature)) + geom_smooth(se = FALSE)+
  labs(title= "Fall", x="", y="")+theme(legend.position = "none")
Fall_means_plot

Fall_means_plot2<- ggplot(FTmeans, aes(Year, Temperature)) + geom_col()+
  labs(title= "Fall")+ geom_smooth(method=lm, se = FALSE, col= "red") 

Fall_means_plot2

##### Summer graph - annual temp means by site ####

SMTmeans <-Summertemp %>%
  group_by(Lat, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(SMTmeans)

Summer_means_plot <- ggplot(SMTmeans, aes(Year, Temperature)) + geom_smooth(se=TRUE, method=lm)+
  labs(title= "Summer", x="", y="") +theme(legend.position = "none")
Summer_means_plot

#### winter graph - annual temperature means by site #####

WTmeans <-Wintertemp %>%
  group_by(Lat, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(WTmeans)

Winter_means_plot <- ggplot(WTmeans, aes(Year, Temperature)) + geom_smooth(se = FALSE)+
  labs(title= "Winter", x="", y="") + theme(legend.position = "none")
Winter_means_plot

library(ggplot2)
library(gridExtra)

Spring_means_plot+Summer_means_plot+Fall_means_plot+Winter_means_plot + theme(legend.position = "none", )

plot1<- grid.arrange(Spring_means_plot, Summer_means_plot, Fall_means_plot, Winter_means_plot, left= "Temperature", bottom = "Year")


##### SALINITY ######
View(Master_SALINITY)
Springsal <- Merged[Merged$Month == "Mar" | Merged$Month== "Apr"|
                           Merged$Month == "May",]
View(Springtemp)

Summersal  <- Merged[Merged$Month == "Jun" | Merged$Month== "Jul"|
                            Merged$Month == "Aug",]
View(Summertemp)

Fallsal <-Merged[Merged$Month == "Sept" | Merged$Month== "Oct"|
                        Merged$Month == "Nov",]

Wintersal<-Merged[Merged$Month == "Dec" | Merged$Month== "Jan"|
                         Merged$Month == "Feb",]


#### Spring graph - annual means by and site ####

SSmeans <-Springsal %>%
  group_by(Lat, Year) %>%
  summarize(PPT = mean(SALINITY))
View(SSmeans)

Spring_means_plot2 <- ggplot(SSmeans, aes(Year,PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Spring ", y="",x="")+theme(legend.position = "none")
Spring_means_plot2


##### Fall graph - annual salinity means by site ######

FSmeans <-Fallsal %>%
  group_by(Lat, Year) %>%
  summarize(PPT = mean(SALINITY))
View(FSmeans)

Fall_means_plot2 <- ggplot(FSmeans, aes(Year, Lat, color= PPT)) + geom_point()+
  labs(title= "Fall ", y="",x="")
Fall_means_plot2


##### Summer graph - annual salinity means by site ####

SMSmeans <-Summersal %>%
  group_by(Lat, Year) %>%
  summarize(PPT = mean(SALINITY))
View(SMTmeans)

Summer_means_plot2 <- ggplot(SMSmeans, aes(Year, PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Summer ", y="",x="") +theme(legend.position = "none")
Summer_means_plot2

#### winter graph - annual salinity means by site #####

WSmeans <-Wintersal %>%
  group_by(Lat, Year) %>%
  summarize(PPT = mean(SALINITY))
View(WSmeans)

Winter_means_plot2 <- ggplot(WSmeans, aes(Year, PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Winter ", y="",x="") + theme(legend.position = "none")
Winter_means_plot2

Spring_means_plot2 +Summer_means_plot2+Fall_means_plot2+Winter_means_plot2 + theme(legend.position = "right")

plot3<- grid.arrange(Spring_means_plot2, Summer_means_plot2, Fall_means_plot2, Winter_means_plot2, left= "Salinity (PPT)", bottom = "Year")+theme(plot.margin = unit(c(1, 1, "cm")))
plot3

library(tidyverse)
View(Merged)
### SALINITY ###
Spring1<- ggplot(SSmeans, aes(Year, Lat, color = PPT))+geom_point()+ theme_minimal()+labs(title= "Spring ", y="",x="") +theme(legend.position = "")+scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 15, limits=c(0, 28))
Spring1

Summer1<- ggplot(SMSmeans, aes(Year, Lat, color = PPT))+geom_point()+ theme_minimal()+labs(title= "Summer ", y="",x="") +theme(legend.position = "")+scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 15, limits=c(0, 28))
Summer1

Fall1<- ggplot(FSmeans, aes(Year, Lat, color = PPT))+geom_point()+ theme_minimal()+labs(title= "Fall ", y="",x="") +theme(legend.position = "")+scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 15, limits=c(0, 28))
Fall1

Winter1<-ggplot(WSmeans, aes(Year, Lat, color=PPT)) + geom_point()+ theme_minimal()+labs(title= "Winter ", y="",x="") +theme(legend.position = "")+scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 15, limits=c(0, 28))
Winter1

Latplot1<- ggarrange(Spring1+ rremove("ylab") + rremove("xlab"),Summer1+ rremove("ylab") + rremove("xlab"), Fall1+ rremove("ylab") + rremove("xlab"),Winter1+ rremove("ylab") + rremove("xlab"), nrow=2, ncol=2, common.legend = TRUE, legend= "right" )

Latplot1

require(grid)

annotate_figure(Latplot1, left = textGrob("Latitude", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Year", gp = gpar(cex = 1.3)))

View(STmeans)
## TEMPERATURE ##
Spring2<- ggplot(STmeans, aes(Year, Lat, color = Temperature))+geom_point()+ theme_minimal()+labs(title= "Spring ", y="",x="") +scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 14, limits=c(10, 18))
Spring2

Summer2<- ggplot(SMTmeans, aes(Year, Lat, color = Temperature))+geom_point()+ theme_minimal()+labs(title= "Summer ", y="",x="") +scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 27, limits=c(24, 30))
Summer2

Fall2<- ggplot(FTmeans, aes(Year, Lat, color = Temperature))+geom_point()+ theme_minimal()+labs(title= "Fall ", y="",x="") +scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 15, limits=c(10, 20))
Fall2

Winter2<-ggplot(WTmeans, aes(Year, Lat, color=Temperature)) + geom_point()+ theme_minimal()+labs(title= "Winter ", y="",x="") +scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 6, limits=c(2, 10))
Winter2

Latplot2<- ggarrange(Spring2+ rremove("ylab") + rremove("xlab"),Summer2+ rremove("ylab") + rremove("xlab"), Fall2+ rremove("ylab") + rremove("xlab"),Winter2+ rremove("ylab") + rremove("xlab"), nrow=2, ncol=2 )

Latplot2

require(grid)

annotate_figure(Latplot2, left = textGrob("Latitude", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Year", gp = gpar(cex = 1.3)))

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

#### QUANTILES vs Prevalence ###

Q90_T<- ggplot( Merged.data_Q, aes(T_Q90, Prevalence)) + geom_point()+ geom_smooth(se=FALSE, method=lm)+labs(title= "Q90 TEMP")
Q90_T

Q10_T<- ggplot( Merged.data_Q, aes(T_Q10, Prevalence)) + geom_point()+ geom_smooth(se=FALSE, method=lm)+labs(title= "Q10 TEMP")
Q10_T

Q90_S<- ggplot( Merged.data_Q, aes(S_Q90, Prevalence)) + geom_point()+ geom_smooth(se=FALSE, method=lm)+labs(title= "Q90 SALINITY")
Q90_S

Q10_S<- ggplot( Merged.data_Q, aes(S_Q10, Prevalence)) + geom_point()+ geom_smooth(se=FALSE, method=lm)+labs(title= "Q10 SALINITY")
Q10_S
 
TvS<- ggplot(Merged.data_Q, aes(T_Q90, S_Q10))+ geom_point() + geom_smooth(se= FALSE, method = lm )
TvS


###### Regions most impacted by disease ###############
View(Perkinsus)
Perkinsus$oysteryear=ifelse(Perkinsus$Month== "Nov"| Perkinsus$Month=="Dec", Perkinsus$Year+1, Perkinsus$Year)
head(Perkinsus)


mean_prev_reg <- Perk2 %>%
  dplyr::group_by(Region, oysteryear, State) %>%
  dplyr::summarise(mean_prevalence = mean(Prevalence))

mean_prev_reg
library(wesanderson)

mean_prev_reg <- filter(mean_prev_reg, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')

#custom_palette <- rainbow(29)

region_p<-  ggplot(mean_prev_reg, aes(oysteryear, mean_prevalence)) + geom_line(aes(color=Region)) + 
  facet_wrap(Region~.) + theme_classic()+geom_smooth(method= lm, se=FALSE)
 #+scale_color_manual(values = custom_palette)

region_p

