setwd("~/Documents/UMBC/Meta-Analysis")

#### load libraries ###
library(readxl)
library(ggplot2)
library(scales)
library(zoo)


#### open environmental data ####
EnvMD<- read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VAupdated.xlsx",)

Master <- na.omit(EnvMD)
Master
View(Master)

EnvVA <- read_excel("~/Documents/UMBC/Meta-Analysis/Env_VA.xlsx",)
View(EnvVA)

EnvVA2<- na.omit(EnvVA)
View(EnvVA2)

### assign data as factors ###

## MD ##

Master$MonitoringLocation <- as.factor(Master$MonitoringLocation)
Master$MonitoringLocation

Master$Month <- as.factor(Master$Month)
Master$Month

Master$Parameter <-as.factor(Master$Parameter)

class(Master$Value)

Master$MeasureValue <- as.numeric(Master$MeasureValue)

## VA ##

EnvVA2$MonitoringLocation<-as.factor(EnvVA2$MonitoringLocation)

EnvVA2$Month <- as.factor(EnvVA2$Month)

EnvVA2$Parameter <-as.factor(EnvVA2$Parameter)

EnvVA2$MeasureValue <- as.numeric(EnvVA2$MeasureValue)

#### Subset data ####
### MD ###
names(Master)
Master_PH <- Master[Master$Parameter == "PH",]
Master_PH

Master_SALINITY <- Master[Master$Parameter == "SALINITY",]
Master_SALINITY

Master_WTEMP <- Master[Master$Parameter == "WTEMP",]
Master_WTEMP

### VA ###
EnvVA_WTEMP <- EnvVA2[EnvVA2$Parameter == "WTEMP",]
EnvVA_SALINITY <-EnvVA2[EnvVA2$Parameter == "SALINITY",]
EnvVA_PH <- EnvVA2[EnvVA2$Parameter == "PH",]

### plotting env data by parameter ####

PH_plot <- ggplot(Master_PH,
                  aes(x = Year, y = MeasureValue, color = MonitoringLocation), ylim = c(0,10)) +
    geom_point() +
    labs( title = "Annual Water pH at Monitoring stations in MD",
          x = "Year (August - November)",
          y = "pH at 0.5 meters"
    )

PH_plot


SAL_plot  <- ggplot(Master_SALINITY,
                    aes(x = Year, y = MeasureValue, color = MonitoringLocation), ylim= c())+
    geom_point() +
    labs( title = "Annual Water Salinity at Monitoring stations in MD",
          x = "Year (August - November)",
          y = "pH at 0.5 meters"
    )
SAL_plot


WTEMP_plot <- ggplot(Master_WTEMP,
          aes(x = Year, y = MeasureValue, color = MonitoringLocation)) +
    geom_point() +
    labs( title = "Annual Water Temperature at Monitoring stations in MD",
          x = "Year (August - November)",
          y = "pH at 0.5 meters"
    )
WTEMP_plot

##### DATES ######
### MD ###
Master$SampleDate

strDates <- Master$SampleDate
dates <- as.Date(strDates, "%m/%d/%y")
dates

strDates <- as.character(dates)
strDates

Master$SampleDate <- strDates
Master$SampleDate

View(Master)

### Creating Month factor ###
Master$Month <- format(strDates, format ="%m")
Master$Month

### Create month ###
library(lubridate)
Master$Month <-month(Master$SampleDate)
Master$Month

### add month column to data ###
Master_1 <- Master
Master_1["Month"] <- Master$Month
Master_1
View(Master_1)

### Creating year factor ###
Master_1$Year <- format(strDates, format ="%Y")
Master_1$Year

### Create year ###
library(lubridate)
Master_1$Year <-year(Master$SampleDate)
Master_1$Year
head(Master_1)

View(Master_1)
### add year column to data ###
Master_1 <- Master
Master_1["Year"] <- Master$Year
Master_1
View(Master_1)

## resubset data with new month column ###

names(Master_1)
Master_PH <- Master_1[Master_1$Parameter == "PH",]
Master_PH

Master_SALINITY <- Master_1[Master_1$Parameter == "SALINITY",]
Master_SALINITY

Master_WTEMP <- Master_1[Master_1$Parameter == "WTEMP",]
Master_WTEMP
View(Master_WTEMP)


#### VA dates ###
View(EnvVA2$SampleDate)

strDates2 <- EnvVA2$SampleDate
dates2 <- as.Date(strDates2, "%m/%d/%Y")
dates2

strDates2 <- as.character(dates2)
strDates2

EnvVA2$SampleDate <- strDates2
EnvVA2$SampleDate

View(EnvVA2)

### Creating Month factor ###
EnvVA2$Month <- format(strDates2, format ="%m")
EnvVA2$Month

### Create month ###
library(dplyr)
library(lubridate)
EnvVA2$Month <-month(EnvVA2$SampleDate)
EnvVA2$Month

EnvVA2$Year <-year(EnvVA2$SampleDate)
EnvVA2


### add month column to data ###
EnvVA2_1 <- EnvVA2
EnvVA2_1["Month"] <- EnvVA2$Month
EnvVA2_1
View(EnvVA2_1)

EnvVA2_1 <- EnvVA2
EnvVA2_1["Year"] <- EnvVA2$Year
EnvVA2_1
View(EnvVA2_1)


## resubset data with new month column ###

names(Master_1)
EnvVA2_PH <- EnvVA2_1[EnvVA2_1$Parameter == "PH",]
EnvVA2_PH

EnvVA2_SALINITY <- EnvVA2_1[EnvVA2_1$Parameter == "SALINITY",]
EnvVA2_SALINITY

EnvVA2_WTEMP <-EnvVA2_1[EnvVA2_1$Parameter == "WTEMP",]
EnvVA2_WTEMP
View(Master_WTEMP)

#### open perkinsus data ###

Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA.csv",)
View(Perkinsus)

PM <-na.omit(Perkinsus)
View(PM)

names(PM)
PM_plot <- ggplot(PM,
                     aes(x = Year, y = Prevalence, color = Site)) +
  geom_point() +
  labs( title = "Annual Perkinsus Marinus Prevalence in MD",
        x = "Year",
        y = "Prevalence %"
  ) 
PM_plot 

P_plot <- ggplot(PM,
                 aes(x = Year, y = Prevalence, color = MonitoringStation)) +
  geom_point() +
  labs( title = "Annual Perkinsus Marinus Prevalence in MD",
        x = "Year",
        y = "Prevalence % by monitoring station")
P_plot



### mapping ###

library("tmap")
library("tmaptools")
library("sf")
library("leaflet")


library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


## plotting chesapeake bay - found online ##
## URL : https://rpubs.com/narinderps1991/ChesapeakeBay ##
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)

### Perkinsus Sites ###
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
plot<-ggplot(data = worldMap) +geom_sf(fill="light grey") + coord_sf(xlim = c(-78, -75), ylim = c(37, 40),expand = TRUE)+xlab("Longitude") +ylab("Latitude") +ggtitle("Chesapeake Bay")
plot

plot<-plot+theme(panel.background = element_rect(fill = "white")) 

plot <-plot+annotate(geom = "text",x = -76.1,y = 37.5,label = "Chesapeake Bay",color = "grey",size = 3, angle=90, fontface = "italic")
plot
plot<-plot +annotation_north_arrow(location = "tl",pad_x = unit(0.5, "cm"),pad_y = unit(1, "cm"),height=unit(1,"cm"),width=unit(0.5,"cm")) 
plot  
plot<- plot+theme(panel.grid.major = element_line(linetype = "dashed",color = "dark grey" ,size = 0.2))
plot

Perkinsus$Lat <- as.numeric(Perkinsus$Lat)
Perkinsus$Long <- as.numeric(Perkinsus$Long)


plot <- plot+geom_point(data = Perkinsus,aes(Long, Lat, color = Site ))+theme(legend.position = "none")
plot


### Environmental Sites VA ####
library(readxl)
EnvALL<- read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VA.xlsx")

worldMap <- ne_countries(scale = "medium", returnclass = "sf")
plot2<-ggplot(data = worldMap) +geom_sf(fill="light grey") + coord_sf(xlim = c(-78, -75), ylim = c(37, 40),expand = TRUE)+xlab("Longitude") +ylab("Latitude") +ggtitle(" Chesapeake Bay ENV")
plot

plot2<-plot2+theme(panel.background = element_rect(fill = "white")) 

plot2 <-plot2+annotate(geom = "text",x = -76.1,y = 37.5,label = "Chesapeake Bay",color = "grey",size = 3, angle=90, fontface = "italic")
plot2
plot2<-plot2 +annotation_north_arrow(location = "tl",pad_x = unit(0.5, "cm"),pad_y = unit(1, "cm"),height=unit(1,"cm"),width=unit(0.5,"cm")) 
plot2  
plot2<- plot2+theme(panel.grid.major = element_line(linetype = "dashed",color = "dark grey" ,size = 0.2))
plot2

EnvALL$Latitude <- as.numeric(EnvALL$Latitude)
EnvALL$Longitude <- as.numeric(EnvALL$Longitude)


plot2 <- plot2+geom_point(data = EnvALL,aes(Longitude, Latitude, color = MonitoringLocation))+ theme(legend.position ="none")
plot2




#### plot WTEMP vs prevalence ##
library(ggplot2)
library(dplyr)
names(Master_WTEMP)


  
## temperature by month & year 1984-2019 ##
Temp_plot <-ggplot(data = Master_WTEMP, aes(Month, MeasureValue, color = factor(Year))) + geom_smooth() +
   labs(title = "Monthly Temperature in the Chesapeake Bay 1984-2019",
                      y= "Temperature (deg C)") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                                             "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec"))
 
Temp_plot 



## Salinity by month & year 1984-2019 ##
Sal_plot <-ggplot(data = Master_SALINITY, aes(Month, MeasureValue, color = factor(Year))) +
  geom_point() + labs(title = "Monthly Salinity in the Chesapeake Bay 1984-2019",
                      y= "Salinity (ppt)") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                                             "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec"))


Sal_plot

## pH by month & year 1984-2019 ##
pH_plot <-ggplot(data = Master_PH, aes(Month, MeasureValue, color = factor(Year))) +
  geom_point() + labs(title = "Monthly Salinity in the Chesapeake Bay 1984-2019",
                      y= "pH") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                                        "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec"))

pH_plot


##Subseting Environmental data by year ##

Year1984 <- Master_1[Master_1$Year == "1984",]
Year1984

Year1985 <- Master_1[Master_1$Year == "1985",]
Year1985

Year1986 <- Master_1[Master_1$Year == "1986",]
Year1986

Year1987 <- Master_1[Master_1$Year == "1987",]
Year1987

Year1988 <- Master_1[Master_1$Year == "1988",]
Year1988

Year1989 <- Master_1[Master_1$Year == "1989",]
Year1989

Year1990 <- Master_1[Master_1$Year == "1990",]
Year1990

Year1991 <- Master_1[Master_1$Year == "1991",]
Year1991

Year1992 <- Master_1[Master_1$Year == "1992",]
Year1992

Year1993 <- Master_1[Master_1$Year == "1993",]
Year1993

Year1994 <- Master_1[Master_1$Year == "1994",]
Year1994

Year1995 <- Master_1[Master_1$Year == "1995",]
Year1995

Year1996 <- Master_1[Master_1$Year == "1996",]
Year1996

Year1997 <- Master_1[Master_1$Year == "1997",]
Year1997

Year1998 <- Master_1[Master_1$Year == "1998",]
Year1998

Year1999 <- Master_1[Master_1$Year == "1990",]
Year1999

Year2000 <- Master_1[Master_1$Year == "2000",]
Year2000

Year2001 <- Master_1[Master_1$Year == "2001",]
Year2001

Year2002 <- Master_1[Master_1$Year == "2002",]
Year2002

Year2003 <- Master_1[Master_1$Year == "2003",]
Year2003

Year2004 <- Master_1[Master_1$Year == "2004",]
Year2004

Year2005 <- Master_1[Master_1$Year == "2005",]
Year2005

Year2006 <- Master_1[Master_1$Year == "2006",]
Year2006

Year2007 <- Master_1[Master_1$Year == "2007",]
Year2007

Year2008 <- Master_1[Master_1$Year == "2008",]
Year2008

Year2009 <- Master_1[Master_1$Year == "2009",]
Year2009

Year2010 <- Master_1[Master_1$Year == "2010",]
Year2010

Year2011 <- Master_1[Master_1$Year == "2011",]
Year2011

Year2012 <- Master_1[Master_1$Year == "2012",]
Year2012

Year2013 <- Master_1[Master_1$Year == "2013",]
Year2013

Year2014 <- Master_1[Master_1$Year == "2014",]
Year2014

Year2015 <- Master_1[Master_1$Year == "2015",]
Year2015

Year2016 <- Master_1[Master_1$Year == "2016",]
Year2016

Year2017 <- Master_1[Master_1$Year == "2017",]
Year2017

Year2018 <- Master_1[Master_1$Year == "2018",]
Year2018

Year2019 <- Master_1[Master_1$Year == "2019",]
Year2019

##Subseting Perkinsus data by year ##

P_1990 <- PM[PM$Year == "1990", ]
P_1991 <- PM[PM$Year == "1991", ]
P_1992 <- PM[PM$Year == "1992", ]
P_1993 <- PM[PM$Year == "1993", ]
P_1994 <- PM[PM$Year == "1994", ]
P_1995 <- PM[PM$Year == "1995", ]
P_1996 <- PM[PM$Year == "1996", ]
P_1997 <- PM[PM$Year == "1997", ]
P_1998 <- PM[PM$Year == "1998", ]
P_1999 <- PM[PM$Year == "1999", ]
P_2000 <- PM[PM$Year == "2000", ]
P_2001 <- PM[PM$Year == "2001", ]
P_2002 <- PM[PM$Year == "2002", ]
P_2003 <- PM[PM$Year == "2003", ]
P_2004 <- PM[PM$Year == "2004", ]
P_2005 <- PM[PM$Year == "2005", ]
P_2006 <- PM[PM$Year == "2006", ]
P_2007 <- PM[PM$Year == "2007", ]
P_2008 <- PM[PM$Year == "2008", ]
P_2009 <- PM[PM$Year == "2009", ]
P_2010 <- PM[PM$Year == "2010", ]
P_2011 <- PM[PM$Year == "2011", ]
P_2012 <- PM[PM$Year == "2012", ]
P_2013 <- PM[PM$Year == "2013", ]
P_2014 <- PM[PM$Year == "2014", ]
P_2015<- PM[PM$Year == "2015", ]
P_2016 <- PM[PM$Year == "2016", ]
P_2017 <- PM[PM$Year == "2017", ]
P_2018 <- PM[PM$Year == "2018", ]
P_2019 <- PM[PM$Year == "2019", ]

View(P_2019)
##Subseting Temperature data by year ##

Temp2017 <- Master_1[Master_1$Year == "2017"|Master_1$Parameter == "WTEMP",]
Temp2017
View(Temp2017)


## Parameters by month & year - need to imput specific year  ##
x_plot <-ggplot(data = Year2017, aes(Month, MeasureValue, color= Parameter)) + geom_smooth()+
  labs(title = "Average Monthly Environmental data in the Chesapeake Bay 2017",
       y= "Temperature (deg C)") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                              "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) 

x_plot  


### Graph temperature by month & year all sites- need to imput specific subsetted year ###
Temp_plot <-ggplot(data = Temp2017, aes(Month, MeasureValue)) + geom_smooth()+
  labs(title = "Average Monthly Temperature in the Chesapeake Bay 2017",
       y= "Temperature (deg C)") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                              "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) 

Temp_plot  


### Graph temperature by month & year all sites- need to imput specific subsetted year ###
Temp2_plot <-ggplot(data = Temp2017, aes(Month, MeasureValue)) + geom_point(alpha= 2, aes(color = MonitoringLocation)) +
  labs(title = "Monthly Temperature in the Chesapeake Bay 2017 by monitoring location",
       y= "Temperature (deg C)") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                              "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) 

Temp2_plot 


## GRID perkinsus all sites seperately ##
perk_plot <- ggplot(Perkinsus, aes(Year, Prevalence)) + geom_smooth() +  facet_wrap("Site")+
  labs(title="Average yearly Perkinsus Marinus Prevalence Chesapeake Bay", y="Prevalence (%)")
perk_plot

### GRID Temp data all sites separately by month/all years - can substitute specific year ##
Temp3_plot <- ggplot(data = Master_WTEMP, aes(Month, MeasureValue)) + geom_smooth() + facet_wrap("MonitoringLocation")+
  labs(title= "Average monthly temperature all sites Chesapeake Bay", y= "Temperature (deg C)") + scale_x_discrete(limit = c("1"="Jan", "2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                                                                                                                               "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) 
Temp3_plot


######## 4/28/22 ##########################################

## yearly means of all sites MD Chesapeake Bay ###
Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA.csv",)
View(Perkinsus)

PM<-na.omit(Perkinsus)
View(PM)
PM$Prevalence<- as.numeric(PM$Prevalence)

library(lubridate)
library(dplyr)

PM_means<-PM %>%
  group_by(Year) %>%
  summarize(Mean.Intensity = mean(Mean.Intensity))
View(PM_means)

PM_means<-PM %>%
  group_by(Year) %>%
  summarize(Prevalence = mean(Prevalence))
View(PM_means)
warnings()

library(ggplot2)
### Plotting Annual Mean P. Marinus Prev % in MD Chesapeake Bay ###
PM_plot2 <- ggplot(PM_means, aes(Year, Mean.Intensity)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual mean P. marinus Mean Infection Intensity in Chesapeake Bay", y = "Mean Infection intensity") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red")
PM_plot2


## yearly means temperature all sites MD ###
wtemp_means<-Master_WTEMP %>%
  group_by(Year) %>%
  summarize(MeanTemperature = mean(MeasureValue))
wtemp_means


## removing 1984-1989 (unneeded years)###
wtemp_means<-wtemp_means[-c(1,2,3,4,5,6),]
wtemp_means

## yearly means temperature all sites VA ##
EnvVA2_WTEMP
wtemp_means2<-EnvVA2_WTEMP %>%
  group_by(Year) %>%
  summarize(MeanTemperature = mean(MeasureValue))
wtemp_means2

## plotting Annual mean temperature in MD Chesapeake Bay ###
WTEMP_plot2 <- ggplot(wtemp_means, aes(Year, MeanTemperature)) + 
  labs(title= "Annual mean Temperature in MD Chesapeake", y = "Temperature (deg C") +
  theme(axis.text.x = element_text(angle= 45)) +  geom_smooth( se=FALSE, col = "red")
WTEMP_plot2

## plotting Annual mean temperature in VA Chesapeake Bay ###
WTEMP_plot2 <- ggplot(wtemp_means2, aes(Year, MeanTemperature)) + 
  labs(title= "Annual mean Temperature in VA Chesapeake", y = "Temperature (deg C") +
  theme(axis.text.x = element_text(angle= 45)) +  geom_smooth( se=FALSE, col = "red")
WTEMP_plot2

### yearly means by site temperature ####
Tsite_means<- Master_WTEMP %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(MeanTemperature = mean(MeasureValue))
Tsite_means


Tsite_plot<- ggplot(Tsite_means, aes(Year, MeanTemperature, color = MonitoringLocation)) + 
  labs(title= "Annual mean Temperature by site in MD Chesapeake", y = "Temperature (deg C") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(se=FALSE)
Tsite_plot



## plotting Annual mean salinity in MD Chesapeake Bay ### 
## yearly means all sites MD ###
salinity_means<-Master_SALINITY %>%
  group_by(Year) %>%
  summarize(MeanSalinity = mean(MeasureValue))
salinity_means

## removing 1984-1989 (unneeded years)###
salinity_means<-salinity_means[-c(1,2,3,4,5,6),]
salinity_means

SAL_plot2 <- ggplot(salinity_means, aes(Year, MeanSalinity)) + 
  labs(title= "Annual mean Salinity in MD Chesapeake", y = "Salinity (ppt)") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(se = FALSE,col= "red")
SAL_plot2


## plotting annual mean pH in MD Chesapeake Bay ###
## yearly means all sites MD ###
ph_means<-Master_PH %>%
  group_by(Year) %>%
  summarize(Meanph = mean(MeasureValue))
ph_means

## removing 1984-1989 (unneeded years)###
ph_means<-ph_means[-c(1,2,3,4,5,6),]
ph_means

PH_plot2 <- ggplot(ph_means, aes(Year, Meanph)) + 
  labs(title= "Annual mean pH in MD Chesapeake", y = "pH") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(se = FALSE,col= "red")
PH_plot2


### reading Perkinsus VA  data ###

PerkinsusVA <- read.csv("~/Documents/UMBC/Meta-Analysis/Perkinsus VA data.csv",)
PerkinsusVA

names(PerkinsusVA)


PerkinsusVA$Year <-as.factor(PerkinsusVA$Year)
PerkinsusVA$Month<-as.factor(PerkinsusVA$Month)
PerkinsusVA$Region<-as.factor(PerkinsusVA$Region)
PerkinsusVA$State<-as.factor(PerkinsusVA$State)
PerkinsusVA$Site<-as.factor(PerkinsusVA$Site)
PerkinsusVA$Prevalence<-as.numeric(PerkinsusVA$Prevalence)
PerkinsusVA$Collected<-as.factor(PerkinsusVA$Collected)
PerkinsusVA$Mean.I.Heavy<-as.factor(PerkinsusVA$Mean.I.Heavy)
PerkinsusVA$Mean.I.Moderate<-as.factor(PerkinsusVA$Mean.I.Moderate)
PerkinsusVA$Mean.I.Light<-as.factor(PerkinsusVA$Mean.I.Light)
PerkinsusVA$Mean.I.Rare<-as.factor(PerkinsusVA$Mean.I.Rare)

Prevalence<- subset(PerkinsusVA, select = -c(Mean.I.Heavy,Mean.I.Moderate,Mean.I.Light,Mean.I.Rare))

Prevalence

PVA<-na.omit(Prevalence)
PVA

PVA$Prevalence<-as.numeric(PVA$Prevalence)
View(PVA)

PV_means <-PVA %>%
  group_by(Year) %>%
  summarize(Prevalence = mean(Prevalence))
PV_means

### Plotting Annual Mean P. Marinus Prev % in VA  Chesapeake Bay ###
PV_plot <- ggplot(PV_means, aes(Year, Prevalence)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual mean P. marinus Prevalence in VA Chesapeake", y = "Prevalence (%)") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red")
PV_plot


#### MD & VA Chesapeake ######


Prev_ALL <- read.csv("~/Documents/UMBC/Meta-Analysis/Perkinsus Prev Chesapeake 1990-2021.csv",)
Prev_ALL

names(Prev_ALL)

Prev_ALL$Month<-as.factor(Prev_ALL$Month)
View(Prev_ALL)

Prev_ALL<-subset(Prev_ALL, select = -c(Month))
Prev_ALL

PrevALL<-na.omit(Prev_ALL)
PrevALL


PrevALL$Prevalence<-as.numeric(PrevALL$Prevalence)
View(PrevALL)

P_means <-PrevALL %>%
  group_by(Year) %>%
  summarize(Prevalence = mean(Prevalence))
View(P_means)


### Plotting Annual Mean P. Marinus Prev % in MD Chesapeake Bay ###
P_plot <- ggplot(P_means, aes(Year, Prevalence)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= "Annual mean P. marinus Prevalence in Chesapeake Bay", y = "Prevalence (%)") +
  theme(axis.text.x = element_text(angle= 45)) + geom_smooth(method=lm, se = FALSE, col= "red")

P_plot

#### 5/2/22 - Overlaying graphs ####
library(patchwork)
library(hrbrthemes)
library(tidyr)

 ### MD Grid ### 
WTEMP_plot2 + PM_plot2 + SAL_plot2 + PH_plot2

#### MD prevalence by year and site ###

Pmeans <-P_1990 %>%
  group_by(Site) %>%
  summarize(Prevalence = mean(Prevalence))
View(Pmeans)


P1990_plot <- ggplot(P_1990, aes(Site, Prevalence)) + geom_col(color= "grey", fill= "grey") + 
  labs(title= " Mean P. marinus Prevalence MD by site 1990", y = "Prevalence (%)") +
  theme(axis.text.x = element_text(angle= 90)) 
P1990_plot



######## 5/5/22 #######

### GIS map Perkinsus sites MD ###

options(repr.plot.width=20, repr.plot.height=15)

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

map_new <- st_read("~/Documents/UMBC/Meta-Analysis/ChesapeakeBay/Chesapeake_Bay_Shoreline_High_Resolution/", "Chesapeake_Bay_Shoreline_High_Resolution")
map_new

options(sf_max.plot=1)
plot(map_new, axes=TRUE)

map<-fortify(map_new)

Site_area<-ggplot() + geom_sf(data = map)+theme(panel.grid.minor = element_blank(),panel.background = element_blank())+geom_point(data = EnvALL,aes(Long, Lat, color = Site ))+theme(legend.position="none")
Site_area

View(EnvALL)

##### 5/9/22 #########

#### monthly/yearly comparisons ####

### subset monthly Temperature ###

JulyTemp <- Master_WTEMP[Master_WTEMP$Month == "7",]
View(JulyTemp)

AugTemp <- Master_WTEMP[Master_WTEMP$Month == "8",]
View(AugTemp)

SeptTemp <- Master_WTEMP[Master_WTEMP$Month == "9",]
View(SeptTemp)

OctTemp <- Master_WTEMP[Master_WTEMP$Month == "10",]
View(OctTemp)

NovTemp <- Master_WTEMP[Master_WTEMP$Month == "11",]
View(NovTemp)

### Graphing #### 

JulyTemp_plot <- ggplot(JulyTemp, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "July Temperature MD Chespeake Bay All Sites")+ theme(legend.position = "none")
JulyTemp_plot

AugTemp_plot <- ggplot(AugTemp, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "August Temperature MD Chespeake Bay All Sites") +theme(legend.position = "none")
AugTemp_plot

SeptTemp_plot <- ggplot(SeptTemp, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "Sept Temperature MD Chespeake Bay All Sites")+ theme(legend.position = "none")
SeptTemp_plot

OctTemp_plot <- ggplot(OctTemp, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "October Temperature MD Chespeake Bay All Sites") +theme(legend.position = "none")
OctTemp_plot

NovTemp_plot <- ggplot(NovTemp, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "November Temperature MD Chespeake Bay All Sites") +theme(legend.position = "none")
NovTemp_plot

JulyTemp_plot + AugTemp_plot + SeptTemp_plot + OctTemp_plot + NovTemp_plot 


### subset monthly pH ###

Julyph <- Master_PH[Master_PH$Month == "7",]
View(JulySal)

Augph <- Master_PH[Master_PH$Month == "8",]
View(AugSal)

Septph <- Master_PH[Master_PH$Month == "9",]
View(SeptSal)

Octph <- Master_PH[Master_PH$Month == "10",]
View(OctSal)

Novph <- Master_PH[Master_PH$Month == "11",]
View(NovSal)

### Graphing #### 

Julyph_plot <- ggplot(Julyph, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "July pH MD Chespeake Bay All Sites")+ theme(legend.position = "none")
JulySal_plot

Augph_plot <- ggplot(Augph, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "August pH MD Chespeake Bay All Sites") +theme(legend.position = "none")
Augph_plot

Septph_plot <- ggplot(Septph, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "Sept pH MD Chespeake Bay All Sites")+ theme(legend.position = "none")
Septph_plot

Octph_plot <- ggplot(Octph, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "October pH MD Chespeake Bay All Sites") +theme(legend.position = "none")
Octph_plot

Novph_plot <- ggplot(Novph, aes(Year, MeasureValue, color = MonitoringLocation))+
  geom_smooth(se=FALSE) + labs(title = "November pH MD Chespeake Bay All Sites") +theme(legend.position = "none")
Novph_plot

Julyph_plot + Augph_plot + Septph_plot + Octph_plot + Novph_plot 

########## 5/10/22 #################
names(Master_WTEMP)

T_means <-EnvVA2_WTEMP %>%
  group_by(Month, Year) %>%
  summarize(MeasureValue = mean(MeasureValue))
View(T_means)

Temp_means_plot <- ggplot(T_means, aes(Month, MeasureValue, color = factor(Year))) + geom_smooth(se= FALSE) +
  labs(title= "Mean Monthly Temperature VA Chesapeake Bay", y = "Temperature (deg C)") 
  
Temp_means_plot


S_means <-EnvVA2_SALINITY %>%
  group_by(Month, Year) %>%
  summarize(MeasureValue = mean(MeasureValue))
View(S_means)

S_means_plot <- ggplot(S_means, aes(Month, MeasureValue, color = factor(Year))) + geom_smooth(se= FALSE) +
  labs(title= "Mean Monthly Salinity VA Chesapeake Bay", y = "PPT")
S_means_plot


######### 5/16/22 ################################################

######### SEASONS ###############

#### TEMPERATURE ######

Merged<-read.csv("~/Documents/UMBC/Meta-Analysis/MergedData2.csv",)
View(Merged)

library(ggplot2)

View(Merged.data)

Springtemp <- Merged.data[Merged.data$Month == "Mar" | Merged.data$Month== "Apr"|
                             Merged.data$Month == "May",]
View(Springtemp)

Summertemp  <- Merged.data[Merged.data$Month == "Jun" | Merged.data$Month== "Jul"|
                             Merged.data$Month == "Aug",]
View(Summertemp)

Falltemp <-Merged.data[Merged.data$Month == "Sept" | Merged.data$Month== "Oct"|
                         Merged.data$Month == "Nov",]

Wintertemp<-Merged.data[Merged.data$Month == "Dec" | Merged.data$Month== "Jan"|
                          Merged.data$Month == "Feb",]
View(Wintertemp)

#### Spring graph - annual means by and site ####
library(dplyr)
STmeans <-Springtemp %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(STmeans)

Spring_means_plot <- ggplot(STmeans, aes(Year, Temperature, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Spring", x="", y="")+theme(legend.position = "none")
Spring_means_plot

library(ggplot2)

##### Fall graph - annual temp means by site ######

FTmeans <-Falltemp %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(FTmeans)

Fall_means_plot <- ggplot(FTmeans, aes(Year, Temperature, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Fall", x="", y="")+theme(legend.position = "none")
Fall_means_plot


##### Summer graph - annual temp means by site ####

SMTmeans <-Summertemp %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(SMTmeans)

Summer_means_plot <- ggplot(SMTmeans, aes(Year, Temperature, color = factor(MonitoringLocation))) + geom_smooth(se=FALSE)+
  labs(title= "Summer", x="", y="") +theme(legend.position = "none")
Summer_means_plot

#### winter graph - annual temperature means by site #####

WTmeans <-Wintertemp %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(Temperature = mean(WTEMP))
View(WTmeans)

Winter_means_plot <- ggplot(WTmeans, aes(Year, Temperature, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Winter", x="", y="") + theme(legend.position = "none")
Winter_means_plot

library(ggplot2)
library(gridExtra)

Spring_means_plot+Summer_means_plot+Fall_means_plot+Winter_means_plot + theme(legend.position = "none", )

plot1<- grid.arrange(Spring_means_plot, Summer_means_plot, Fall_means_plot, Winter_means_plot, left= "Temperature", bottom = "Year")


##### SALINITY ######
View(Master_SALINITY)
Springsal <- Merged.data[Merged.data$Month == "Mar" | Merged.data$Month== "Apr"|
                            Merged.data$Month == "May",]
View(Springtemp)

Summersal  <- Merged.data[Merged.data$Month == "Jun" | Merged.data$Month== "Jul"|
                             Merged.data$Month == "Aug",]
View(Summertemp)

Fallsal <-Merged.data[Merged.data$Month == "Sept" | Merged.data$Month== "Oct"|
                         Merged.data$Month == "Nov",]

Wintersal<-Merged.data[Merged.data$Month == "Dec" | Merged.data$Month== "Jan"|
                          Merged.data$Month == "Feb",]


#### Spring graph - annual means by and site ####

SSmeans <-Springsal %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(PPT = mean(SALINITY))
View(SSmeans)

Spring_means_plot2 <- ggplot(SSmeans, aes(Year,PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Spring ", y="",x="")+theme(legend.position = "none")
Spring_means_plot2


##### Fall graph - annual salinity means by site ######

FSmeans <-Fallsal %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(PPT = mean(SALINITY))
View(Fsmeans)

Fall_means_plot2 <- ggplot(FSmeans, aes(Year, PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Fall ", y="",x="")+theme(legend.position = "none")
Fall_means_plot2


##### Summer graph - annual salinity means by site ####

SMSmeans <-Summersal %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(PPT = mean(SALINITY))
View(SMTmeans)

Summer_means_plot2 <- ggplot(SMSmeans, aes(Year, PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Summer ", y="",x="") +theme(legend.position = "none")
Summer_means_plot2

#### winter graph - annual salinity means by site #####

WSmeans <-Wintersal %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(PPT = mean(SALINITY))
View(WSmeans)

Winter_means_plot2 <- ggplot(WSmeans, aes(Year, PPT, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Winter ", y="",x="") + theme(legend.position = "none")
Winter_means_plot2

Spring_means_plot2 +Summer_means_plot2+Fall_means_plot2+Winter_means_plot2 + theme(legend.position = "right")

plot3<- grid.arrange(Spring_means_plot2, Summer_means_plot2, Fall_means_plot2, Winter_means_plot2, left= "Salinity (PPT)", bottom = "Year")+theme(plot.margin = unit(c(1, 1, "cm")))
plot3

##### pH ######
View(Master_PH)
Springph <- Master_PH[Master_PH$Month == "3" | Master_PH$Month== "4"|
                        Master_PH$Month == "5",]


Summerph  <- Master_PH[Master_PH$Month == "6" | Master_PH$Month== "7"|
                         Master_PH$Month == "8",]


Fallph <-Master_PH[Master_PH$Month == "9" | Master_PH$Month== "10"|
                     Master_PH$Month == "11",]


Winterph<-Master_PH[Master_PH$Month == "12" | Master_PH$Month== "1"|
                      Master_PH$Month == "2",]


#### Spring graph - annual means by and site ####

Sphmeans <-Springph %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(pH = mean(MeasureValue))
View(SSmeans)

Spring_means_plot3 <- ggplot(Sphmeans, aes(Year,pH, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Spring mean pH by site MD Chesapeake Bay")+theme(legend.position = "none")
Spring_means_plot3


##### Fall graph - annual  means by site ######

Fphmeans <-Fallph %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(pH = mean(MeasureValue))
View(Fsmeans)

Fall_means_plot3 <- ggplot(Fphmeans, aes(Year, pH, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Fall mean pH by site MD Chesapeake Bay")+theme(legend.position = "none")
Fall_means_plot3


##### Summer graph - annual salinity means by site ####

Sphmeans <-Summerpj %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(pH = mean(MeasureValue))
View(SMTmeans)

Summer_means_plot3 <- ggplot(Sphmeans, aes(Year, pH, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Summer mean pH by site MD Chesapeake Bay") +theme(legend.position = "none")
Summer_means_plot3

#### winter graph - annual salinity means by site #####

Wphmeans <-Winterph %>%
  group_by(MonitoringLocation, Year) %>%
  summarize(pH = mean(MeasureValue))
View(WSmeans)

Winter_means_plot3 <- ggplot(Wphmeans, aes(Year, pH, color = factor(MonitoringLocation))) + geom_smooth(se = FALSE)+
  labs(title= "Winter mean pH by site MD Chesapeake Bay") + theme(legend.position = "none")
Winter_means_plot3

Spring_means_plot3+Summer_means_plot3+Fall_means_plot3+Winter_means_plot3 + theme(legend.position = "right")


####### 5/23/22 #############################

######## Scatter plot Temperature by month and year (all sites combined) #####
meansT <-EnvVA2_WTEMP %>%
  group_by( Month, Year, MonitoringLocation) %>%
  summarize(Temperature = mean(MeasureValue))
meansT

scattertemp1 <- ggplot(meansT, aes(Month, Temperature, color = Year)) + geom_point() 
scattertemp1

scattertemp2 <- ggplot(meansT, aes(Year, Temperature, color = Month)) + geom_point()
scattertemp2

######## Scatter plot Salinity by month and year (all sites combined) #####
meansS <-EnvVA2_SALINITY %>%
  group_by( Month, Year) %>%
  summarize(Salinity = mean(MeasureValue))
meansS

scattersal1 <- ggplot(meansS, aes(Month, Salinity, color = Year)) + geom_point() + geom_smooth()
scattersal1

scattersal2 <- ggplot(meansS, aes(Year, Salinity, color = Month)) + geom_point() + geom_smooth()
scattersal2




##### perkinsus MD by latitude ####
library(ggplot2)
library(dplyr)
View(Perkinsus)

PM<- na.omit(Perkinsus)

Pmeans <-PM %>%
  group_by(Lat, Year) %>%
  summarize(Prevalence = mean(Prevalence), Mean_Intensity = mean(MeanIntensity), Collected = mean(Collected),Infected= mean(Infected))
Pmeans
View(Pmeans)



plot <- ggplot(Pmeans, aes(Year, Lat, color= Prevalence)) + geom_point()+scale_colour_gradientn(colours= rainbow(5)) + labs(title = " Perkinsus means by year and latitude ")
plot



####### geospatial map 5/31/22#####

### prevalence % ###
Site_area2 <-ggplot() + geom_sf(data = map)+
  theme(panel.grid.minor = element_blank(),panel.background = element_blank())+
  geom_point(data = Perkinsus,aes(Long, Lat, color = Prevalence ))+theme(legend.position="none")
Site_area2

library(scatterpie)

Perkinsus$Long <- as.numeric(Perkinsus$Long)
Perkinsus$Prevalence <-as.numeric(Perkinsus$Prevalence)

prevalence_map <-ggplot(data = worldMap) +geom_sf(fill="light grey") +
  coord_sf(xlim = c(-78, -75), ylim = c(37, 40),expand = TRUE)+
  xlab("Longitude") +ylab("Latitude") +ggtitle("1990")+
  theme(panel.background = element_rect(fill = "white"))+
  annotate(geom = "text",x = -76.1,y = 37.5,label = "Chesapeake Bay",color = "grey",size = 3, angle=90, fontface = "italic")+
  annotation_north_arrow(location = "tl",pad_x = unit(0.5, "cm"),pad_y = unit(1, "cm"),height=unit(1,"cm"),width=unit(0.5,"cm"))+
  theme(panel.grid.major = element_line(linetype = "xdashed",color = "dark grey" ,size = 0.2))
p1990<-prevalence_map + geom_scatterpie(data =P_1990 , aes(Long, Lat), cols=c("Collected", "Infected"), color = NA) + theme(legend.position="none")
p1990
p1990+p2000+p2010+p2019                                                                                                       

##### perkinsus ALL by latitude ####
Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA.csv",)
library(ggplot2)
library(dplyr)
library(tidyr)
View(Perkinsus)

Perkinsus2 <- Perkinsus[-c(3)]
Perkinsus2
View(Perkinsus2)

PM<- na.omit(Perkinsus2)
View(PM)

Perk$Prevalence<- as.numeric(Perk$Prevalence)
Perk$Mean.Intensity<-as.numeric(Perk$Mean.Intensity)

Pmeans <- Perk%>%
  group_by(Lat, Year) %>%
  summarize(Prevalence = mean(Prevalence))
Pmeans
View(Pmeans)

Imeans <-Perk %>%
  group_by(Lat, Year) %>%
  summarize(Mean.Intensity = mean(Mean.Intensity))
Imeans

warnings()
plot <- ggplot(Pmeans, aes(Year, Lat, color=Prevalence)) + geom_point()+ theme_minimal()+
  scale_color_gradient2( midpoint = 50,low = "blue", mid = "yellow ", high = " red") + labs(title = " Perkinsus means by year and latitude MD & VA ")
plot

plot1 <- ggplot(Imeans, aes(Year, Lat, color=Mean.Intensity)) + geom_point()+ theme_minimal() +
  scale_color_gradient2(low = "blue", mid = "yellow ", high = "red", midpoint= 3, limits=c(0, 5)) +
  labs(title = " Perkinsus means by year and latitude MD & VA ")
plot1

