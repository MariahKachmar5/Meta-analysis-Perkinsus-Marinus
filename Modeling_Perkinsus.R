
#### Generalized Linear Mixed Model Perkinsus marinus Meta-analysis ####

setwd("~/Documents/UMBC/Meta-Analysis")

install.packages(lme4)
library(lme4)
library(readxl)

Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/Perkinsus_MDVA2.csv",)
Perkinsus

Perk<- na.omit(Perkinsus)

Env<- read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VAupdated.xlsx")
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
View(PH)
PH <- PH %>%
  rename(pH = MeasureValue)

SALINITY <- Env1[Env1$Parameter == "SALINITY",]
SALINITY
SALINITY <- SALINITY %>%
  rename(SALINITY = MeasureValue)


WTEMP <-Env1[Env1$Parameter == "WTEMP",]
View(WTEMP)
WTEMP <- WTEMP %>%
  rename(WTEMP = MeasureValue)

### REMERGING ENVIRONMENTAL DATA ####

MasterENV<- merge(SALINITY, WTEMP, by =c("Year","Month", "MonitoringLocation"), all = TRUE)
View(MasterENV)

MasterENV<-na.omit(MasterENV)
View(MasterENV)

### Yearly means for environmental parameters by site and time ###

library(dplyr)
Yearmeans <-MasterENV %>%
  group_by(Year, WTEMP, SALINITY, MonitoringLocation) %>%
  summarize(WTEMP = mean(WTEMP), SALINITY = mean(SALINITY))
View(Yearmeans)

MonthlyMeans<-Env1 %>%
  group_by(Month, Year, Parameter, Site, Lat, Long) %>%
  summarize(Value = mean(MeasureValue))
View(MonthlyMeans)

### Merging data sheets using Monthly/yearly means  #### 
Master1<- merge(Perk, Yearmeans, by =c("Year","MonitoringLocation"), all = TRUE)
View(Master1)





#### STATISTICS #####

##### NOTES ######
##Fixed parameters - temp means, salinity means, ph means, year
## random variables - sites/lat&long

### GLM Perkinsus data ###

Perkinsus2 <- Perkinsus[-c(3)]
PM<- na.omit(Perkinsus2)
PM
PM$Prevratio=PM$Prevalence/100

PM$Year <- as.numeric(PM$Year)
PM$Lat <- as.numeric(PM$Lat)
PM$Prevalence <- as.numeric(PM$Prevalence)

### using lmer()
model1<-lmer(Prevalence ~ Year + (1|Lat), data = PM)
model1
M1<- plot(model1)
M1

model2<- lmer(Prevalence ~ Lat + (1|Year), PM)
model2



## using glm()
model3<-glm(Prevratio ~ Year+Lat, data = PM, family = binomial)
summary(model3)
M3<-plot(model3)


## looking at the normal Q-Q plot the residuals are deviating from the theoretical distribution
## "thin tails" - under dispersed data? 

model4 <- glm(Prevalence ~ Year, data = PM)
summary(model4)
M4<-plot(model4)
M4
## getting similar normal Q-Q plot as model3. 
## residual vs fitted showing independence?

model5 <- glm(Prevalence ~ Lat, data = PM)
summary(model5)
M5<-plot(model5)
## residual vs fitted showing independence?

model6<- glm(Prevalence~ Year +Site, data = PM)
summary(model6)
plot(model6)
## QQ plot seems to have more of a normal distribution than the other models - thin tails? 
## residuals vs predicted is nonrandom 



###### GLM Environmental Data ######

######## TEMPERATURE ####
WTEMP <- na.omit(WTEMP)
WTEMP$Parameter <- as.numeric(WTEMP$Parameter)
WTEMP$SampleDate <- as.numeric(WTEMP$SampleDate)
WTEMP$MeasureValue <- as.numeric(WTEMP$MeasureValue)
WTEMP$Year <- as.numeric(WTEMP$Year)
WTEMP$Month <- as.numeric(WTEMP$Month)

model7 <- glm(MeasureValue ~ Year, data = WTEMP)
summary(model7)

model8 <- glm(MeasureValue ~ Year + Month, data = WTEMP)
model8

## Error in glm.fit(x = numeric(0), y = numeric(0), weights = NULL, start = NULL,  : 
#object 'fit' not found
#In addition: Warning messages:
#  1: In glm.fit(x = numeric(0), y = numeric(0), weights = NULL, start = NULL,  :
#                  no observations informative at iteration 1
#                2: glm.fit: algorithm did not converge