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

Master

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

## resubset data with new month column ###

names(Master_1)
Master_PH <- Master[Master_1$Parameter == "PH",]
Master_PH

Master_SALINITY <- Master[Master_1$Parameter == "SALINITY",]
Master_SALINITY

Master_WTEMP <- Master[Master_1$Parameter == "WTEMP",]
Master_WTEMP


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
install.packages("tmap")
install.packages("tmaptools")
install.packages("sf")
install.packages("leaflet")
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

Perkinsus$Lat <- as.numeric(Perkinsus$Lat)
Perkinsus$Long <- as.numeric(Perkinsus$Long)


plot <- plot+geom_point(data = Perkinsus,aes(Long, Lat, color = Site ))
plot

names(Perkinsus)


