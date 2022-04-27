setwd("~/Documents/UMBC/Meta-Analysis")

#### load libraries ###
library(readxl)
library(ggplot2)
library(scales)
library(zoo)


#### open environmental data ####
EnvMD<- read_excel("~/Documents/UMBC/Meta-Analysis/Environmental Data All Year V2 .xlsx",)

Master <- na.omit(EnvMD)
Master
View(Master)

### assign data as factors ###

Master$MonitoringLocation <- as.factor(Master$MonitoringLocation)
Master$MonitoringLocation

Master$Month <- as.factor(Master$Month)
Master$Month

Master$Parameter <-as.factor(Master$Parameter)

class(Master$Value)

Master$MeasureValue <- as.numeric(Master$MeasureValue)

#### Subset data ####
names(Master)
Master_PH <- Master[Master$Parameter == "PH",]
Master_PH

Master_SALINITY <- Master[Master$Parameter == "SALINITY",]
Master_SALINITY

Master_WTEMP <- Master[Master$Parameter == "WTEMP",]
Master_WTEMP

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

## resubset data with new month column ###

names(Master_1)
Master_PH <- Master[Master_1$Parameter == "PH",]
Master_PH

Master_SALINITY <- Master[Master_1$Parameter == "SALINITY",]
Master_SALINITY

Master_WTEMP <- Master[Master_1$Parameter == "WTEMP",]
Master_WTEMP
View(Master_WTEMP)


#### open perkinsus data ###

Perkinsus <- read_excel("~/Documents/UMBC/Meta-Analysis/Perkinsus MD Data.xlsx",)
Perkinsus

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
plot<-ggplot(data = worldMap) +geom_sf(fill="light grey") + coord_sf(xlim = c(-78, -75), ylim = c(37, 40),expand = TRUE)+xlab("Longitude") +ylab("Latitude") +ggtitle("Maryland Chesapeake Bay")
plot

plot<-plot+theme(panel.background = element_rect(fill = "white"))

plot <-plot+annotate(geom = "text",x = -76.1,y = 37.5,label = "Chesapeake Bay",color = "grey",size = 3, angle=90, fontface = "italic")
plot
plot<-plot +annotation_north_arrow(location = "tl",pad_x = unit(0.5, "cm"),pad_y = unit(1, "cm"),height=unit(1,"cm"),width=unit(0.5,"cm"))
plot
plot<- plot+theme(panel.grid.major = element_line(linetype = "dashed",color = "dark grey" ,size = 0.2))
plot

PM$Lat <- as.numeric(PM$Lat)
PM$Long <- as.numeric(PM$Long)
View(PM)

plot <- plot+geom_point(data = PM,aes(Long, Lat, color = Site ))
plot


### Environmental Sites ####
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
plot2<-ggplot(data = worldMap) +geom_sf(fill="light grey") + coord_sf(xlim = c(-78, -75), ylim = c(37, 40),expand = TRUE)+xlab("Longitude") +ylab("Latitude") +ggtitle("Maryland Chesapeake Bay")
plot

plot2<-plot2+theme(panel.background = element_rect(fill = "white"))

plot2 <-plot2+annotate(geom = "text",x = -76.1,y = 37.5,label = "Chesapeake Bay",color = "grey",size = 3, angle=90, fontface = "italic")
plot2
plot2<-plot2 +annotation_north_arrow(location = "tl",pad_x = unit(0.5, "cm"),pad_y = unit(1, "cm"),height=unit(1,"cm"),width=unit(0.5,"cm"))
plot2
plot2<- plot2+theme(panel.grid.major = element_line(linetype = "dashed",color = "dark grey" ,size = 0.2))
plot2

Master$Lat <- as.numeric(Master$Lat)
Master$Long <- as.numeric(Master$Long)


plot2 <- plot2+geom_point(data = Master,aes(Long, Lat, color = MonitoringLocation))
plot2



#### plot WTEMP vs prevalence ##
library(ggplot2)
library(dplyr)
names(Master_WTEMP)



## temperature by month & year 1984-2019 ##
Temp_plot <-ggplot(data = Master_WTEMP, aes(Month, MeasureValue, color = factor(Year))) +
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

Year1984 <- Master[Master_1$Year == "1984",]
Year1984

Year1985 <- Master[Master_1$Year == "1985",]
Year1985

Year1986 <- Master[Master_1$Year == "1986",]
Year1986

Year1987 <- Master[Master_1$Year == "1987",]
Year1987

Year1988 <- Master[Master_1$Year == "1988",]
Year1988

Year1989 <- Master[Master_1$Year == "1989",]
Year1989

Year1990 <- Master[Master_1$Year == "1990",]
Year1990

Year1991 <- Master[Master_1$Year == "1991",]
Year1991

Year1992 <- Master[Master_1$Year == "1992",]
Year1992

Year1993 <- Master[Master_1$Year == "1993",]
Year1993

Year1994 <- Master[Master_1$Year == "1994",]
Year1994

Year1995 <- Master[Master_1$Year == "1995",]
Year1995

Year1996 <- Master[Master_1$Year == "1996",]
Year1996

Year1997 <- Master[Master_1$Year == "1997",]
Year1997

Year1998 <- Master[Master_1$Year == "1998",]
Year1998

Year1999 <- Master[Master_1$Year == "1990",]
Year1999

Year2000 <- Master[Master_1$Year == "2000",]
Year2000

Year2001 <- Master[Master_1$Year == "2001",]
Year2001

Year2002 <- Master[Master_1$Year == "2002",]
Year2002

Year2003 <- Master[Master_1$Year == "2003",]
Year2003

Year2004 <- Master[Master_1$Year == "2004",]
Year2004

Year2005 <- Master[Master_1$Year == "2005",]
Year2005

Year2006 <- Master[Master_1$Year == "2006",]
Year2006

Year2007 <- Master[Master_1$Year == "2007",]
Year2007

Year2008 <- Master[Master_1$Year == "2008",]
Year2008

Year2009 <- Master[Master_1$Year == "2009",]
Year2009

Year2010 <- Master[Master_1$Year == "2010",]
Year2010

Year2011 <- Master[Master_1$Year == "2011",]
Year2011

Year2012 <- Master[Master_1$Year == "2012",]
Year2012

Year2013 <- Master[Master_1$Year == "2013",]
Year2013

Year2014 <- Master[Master_1$Year == "2014",]
Year2014

Year2015 <- Master[Master_1$Year == "2015",]
Year2015

Year2016 <- Master[Master_1$Year == "2016",]
Year2016

Year2017 <- Master[Master_1$Year == "2017",]
Year2017

Year2018 <- Master[Master_1$Year == "2018",]
Year2018

Year2019 <- Master[Master_1$Year == "2019",]
Year2019

##Subseting Perkinsus data by year - doesnt work? ##

View(Perkinsus)
names(Perkinsus)

P_1991 <- Master[Perkinsus$Year == "1991", ]
P_1991

View(Master_1)

##Subseting Temperature data by year ##

Temp2017 <- Master[Master_1$Year == "2017"|Master_1$Parameter == "WTEMP",]
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

