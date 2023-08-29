setwd("~/Documents/UMBC/Meta-Analysis")

####################################### yearly means of all sites Chesapeake Bay ###################################
Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA2.csv",)
View(Perkinsus)

PM<-na.omit(Perkinsus)
View(PM)
PM$Prevalence<- as.numeric(PM$Prevalence)

library(lubridate)
library(dplyr)
library(ggplot2)
library(Rmisc)

Prev_means<-PM %>%
  group_by(Year) %>%
  summarize(Prevalence = mean(Prevalence))
View(Prev_means)

PM_means<-PM %>%
  group_by(Year) %>%
  summarize(Mean.Intensity = mean(Mean.Intensity))
View(PM_means)
warnings()


P_sum<- summarySE(PM, "Prevalence", groupvars= c("Year"))
View(P_sum)

I_sum <- summarySE(PM, "Mean.Intensity", groupvars = "Year")
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
Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA2.csv",)
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

Pmeans <- PM%>%
  group_by(, Year) %>%
  summarize(Prevalence = mean(Prevalence))
Pmeans
View(Pmeans)

Imeans <-PM %>%
  group_by(Lat, Year) %>%
  summarize(Mean.Intensity = mean(Mean.Intensity))
Imeans

PerkMeans<-ddply(Merged.data, .(SALINITY,Lat, Year, Month), summarise,
             Prevalence = mean(Prevalence),
             Intensity = mean(Mean.Intensity))
View(PerkMeans)

Feb<-Merged.data %>% filter(Month =='Feb')
Aug<-Merged.data %>% filter(Month =='Aug')
library(ggplot2)
warnings()
plot <- ggplot(Aug, aes(Year, SALINITY, color=Prevalence)) + geom_point()+ theme_minimal()+
  scale_color_gradient2( midpoint = 50,low = "blue", mid = "yellow ", high = " red") + 
  labs(title = "August ")+geom_smooth(se=FALSE)
  
plot

plot1 <- ggplot(PerkMeans, aes(Year, WTEMP, color=Intensity)) + geom_point()+ theme_minimal() +
  scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 3, limits=c(0, 5)) +
  labs(title = " Perkinsus means by year and Temperature MD & VA ")
plot1



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

                      