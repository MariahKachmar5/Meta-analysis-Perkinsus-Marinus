
#### Generalized Linear Mixed Model Perkinsus marinus Meta-analysis ####

setwd("~/Documents/UMBC/Meta-Analysis")

install.packages(lme4)
library(lme4)
library(readxl)

Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/Perkinsus_MDVA.csv",)
Perkinsus

Env<- read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VA.xlsx")
Env


#### creating dates for environmental data ###
View(Env$SampleDate)

strDates2 <- Env$SampleDate
dates2 <- as.Date(strDates2, "%m/%d/%Y")
dates2

strDates2 <- as.character(dates2)
strDates2

Env$SampleDate <- strDates2
Env$SampleDate

View(Env)

### Creating Month factor ###
Env$Month <- format(strDates2, format ="%m")
Env$Month

### Create month ###
library(dplyr)
library(lubridate)
Env$Month <-month(Env$SampleDate)
Env$Month

Env$Year <-year(Env$SampleDate)
Env


### add month column to data ###
Env1 <- Env
Env1["Month"] <- EnvVA2$Month
Env1
View(Env1)

Env1 <- Env
Env1["Year"] <- Env$Year
Env1
View(Env1)

### subsetting environmental data by parameter ###

PH <- Env1[Env1$Parameter == "PH",]
PH

SALINITY <- Env1[Env1$Parameter == "SALINITY",]
SALINITY

WTEMP <-Env1[Env1$Parameter == "WTEMP",]
View(WTEMP)

### Yearly means for environmental parameters by site ###

library(dplyr)
Emeans <-Env1 %>%
  group_by(Year, Parameter, MonitoringLocation, Latitude, Longitude) %>%
  summarize(Value = mean(MeasureValue))
View(Emeans)


##### NOTES ######
##Fixed parameters - temp means, salinity means, ph means, year
## random variables - sites/lat&long


lmer(Prevalence ~ Parameter*Year + MonitoringLocation, data = Emeans)

#Q: how to create model with separate sheets? Should I combine them? 

