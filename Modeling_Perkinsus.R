
#### Generalized Linear Mixed Effects Model Perkinsus marinus Meta-analysis ####

setwd("~/Documents/UMBC/Meta-Analysis")

install.packages(lme4)
library(lme4)
library(readxl)

Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA2.csv",)
View(Perkinsus)

Perk<- na.omit(Perkinsus)
View(Perk)

#Intensity individual data from MDDNR ##
IntensityMD<- read.csv("~/Documents/UMBC/Meta-Analysis/MDDNR_INDV_1990-2015 .csv",)
View(IntensityMD)
Int1990_2015<- subset(IntensityMD, select=c(Region, Name, SiteCode, YearSamp,SampleDate, AnimalNum, DermoIntentNum))
View(Int1990_2015)

write.table(Int1990_2015, file ="MDDNR_1990-2015.xlsx",sep=",", row.names=FALSE)
library(writexl)
write_xlsx(Int1990_2015,"~/Documents/UMBC/Meta-Analysis/MDDNR_1990-2015.xlsx" )

EnvALL<- read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VAupdated.xlsx")
View(EnvALL)
write.table(Env, file="Env.csv", sep=",", row.names=FALSE)

#### creating dates for environmental data ###
View(EnvALL$SampleDate)

strDates2 <- EnvALL$SampleDate
dates2 <- as.Date(strDates2, "%m/%d/%Y")
dates2

strDates2 <- as.character(dates2)
strDates2

EnvALL$SampleDate <- strDates2
EnvALL$SampleDate

View(EnvALL)

### Creating Month factor ###
EnvALL$Month <- format(strDates2, format ="%m")
EnvALL$Month

### Create month ###
library(dplyr)
library(lubridate)
EnvALL$Month <-month(EnvALL$SampleDate)
EnvALL$Month

EnvALL$Year <-year(EnvALL$SampleDate)
EnvALL
View(EnvALL)

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

PH <- EnvALL[EnvALL$Parameter == "PH",]
View(PH)
PH <- PH %>%
  rename(pH = MeasureValue)

SALINITY <- EnvALL[EnvALL$Parameter == "SALINITY",]
SALINITY
SALINITY <- SALINITY %>%
  rename(SALINITY = MeasureValue)


WTEMP <-EnvALL[EnvALL$Parameter == "WTEMP",]
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
Smeans <-MasterENV %>%
  group_by(Year, Month, SALINITY, MonitoringLocation) %>%
  summarize(SALINITY = mean(SALINITY))
View(Smeans)

#### TEMP ###
MonthlyMeans<-MasterENV %>%
  group_by(Month, Year, WTEMP, MonitoringLocation) %>%
  summarize(WTEMP = mean(WTEMP))
View(MonthlyMeans)

MonthlyMeans$Month <- month.abb[MonthlyMeans$Month]

head(MonthlyMeans)
View(MonthlyMeans)

### Salinity ###

MonthlyMeans1<-MasterENV %>%
  group_by(Month, Year, SALINITY, MonitoringLocation) %>%
  summarize(SALINITY = mean(SALINITY))
View(MonthlyMeans1)

MonthlyMeans1$Month <- month.abb[MonthlyMeans1$Month]

head(MonthlyMeans1)
View(MonthlyMeans1)

View(Perk)
library(dplyr)
### Merging data sheets using Monthly/yearly means  #### 
Master1<- merge(Perk, MonthlyMeans, by =c("Year", "MonitoringLocation"))
View(Master1)

Master1<- Master1 %>% 
  rename( "Sample.month"= "Month.x")
View(Master1)

Master1<- Master1 %>% 
  rename( "Month"= "Month.y")
View(Master1)



Merged.data <- merge(MonthlyMeans1, Master1, by =c("Year","Month", "MonitoringLocation"))
View(Merged.data)

Merged.data$oysteryear=ifelse(Merged.data$Month== "Nov"| Merged.data$Month=="Dec", Merged.data$Year+1, Merged.data$Year)

head(Merged.data)

View(Merged.data)

write.table(Merged.data, file="MergedData2.csv", sep=",", row.names=FALSE)

View(Perk)



## Subsetting by month ##

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

### Site Salinity Means & binning L-M-H ###

SiteSalinity<-ddply(Merged.data, .(Site), summarise,
                  SALINITY = mean(SALINITY))
View(SiteSalinity)

SiteDisease<-ddply(Merged.data, .(Site), summarise,
                 Prevalence = mean(Prevalence), Intensity= mean(Mean.Intensity))
View(SiteDisease)

SiteSalDisease <-merge(SiteSalinity, SiteDisease, by =c("Site"))
View(SiteSalDisease)

library(ggplot2)
SalDiseasePlot<- ggplot(SiteSalDisease, aes(SALINITY)) + geom_histogram() 
SalDiseasePlot

##Binning L 0-10, M 10.01 - 20, H 20.01+
SiteSalDisease$Range <- as.factor(ifelse(SiteSalDisease$SALINITY<10, 'LOW',
                                    ifelse(SiteSalDisease$SALINITY >20, 'HIGH', "MEDIUM")))
head(SiteSalDisease)

SiteSalDisease = subset(SiteSalDisease, select = -c(Salinity.Range))

View(SiteSalDisease)

############################### STATISTICS ##################################################################

##### NOTES ######
##Fixed parameters - Temperature, Salinity, Oysteryear, Latitude
## random variables - reef sites, monitoring location

### GLM Perkinsus data ###

View(Perk)
Perkinsus2 <- Perkinsus[-c(3)]
PM<- na.omit(Perkinsus2)
PM
PM$Prevratio=PM$Prevalence/100

Perk$Year <- as.numeric(Perk$Year)
Perk$Lat <- as.numeric(Perk$Lat)
Perk$Prevalence <- as.numeric(Perk$Prevalence)

Perk$oysteryear=ifelse(Perk$Month== "Nov"| Perk$Month=="Dec", Perk$Year+1, Perk$Year)
head(Perk)


### using lmer()
library(car)

### Perkinsus ###


model1<- lmer(Prevalence ~ Lat * oysteryear + (1|Site), data = Perk)
Anova(model1)


model2<- lmer(Mean.Intensity ~ Lat * oysteryear + (1|Site), data = Perk)
Anova(model2)

modelX<-lmer(Prevalence ~ Site * oysteryear + (1|Lat), data= Perk)
Anova(modelX)

modelY<- lmer(Mean.Intensity ~ Site * oysteryear + (1|Lat), data = Perk)
Anova(modelY)

### Environmental Data ####

model5<- lmer(WTEMP ~ Lat * Year + (1|Site), data = Merged.data)
Anova(model5)
  
model6<- lmer(SALINITY ~ Lat * Year + (1|Site), data = Merged.data)
Anova(model6)

###### modeling env + prev ######

library(car)

class(Merged.data$Prevelence)
Merged.data$Prevalence <- as.numeric(Merged.data$Prevalence)
Merged.data$oysteryear <- as.numeric(Merged.data$oysteryear)
Merged.data$WTEMP <- as.numeric(Merged.data$WTEMP)
Merged.data$SALINITY <- as.numeric(Merged.data$SALINITY)
Merged.data$Prevalence <- as.numeric(Merged.data$Prevalence)

class(Merged.data$Site)
View(Merged.data$Site)

View(Merged.data)

model3<- lmer(Prevalence~ oysteryear*  SALINITY * Region+  (1|Site) + (1|MonitoringLocation), Merged.data)
Anova(model3)
summary(model3)
#Effect sizes: 
# Temperature d = 25.66 / 901.35 = .03
# Salinity d = 33.99 / 901.35 = .04

model4<- lmer(Mean.Intensity~ oysteryear* WTEMP* SALINITY*Region + (1|Site) + (1|MonitoringLocation), Merged.data)
Anova(model4)
summary(model4)
#Effect sizes: 
# Temperature d = 1.131 / 1.5461 = .73
# Salinity d = .3565 / 1.5461 = .23

### Collinearity- https://www.codingprof.com/3-ways-to-test-for-multicollinearity-in-r-examples/ ###
library("olsrr") ## error - using wrong model, need lm()
library("corrplot") ## need to be numeric
corrplot(cor(Merged.data), method = "number")

#https://easystats.github.io/blog/posts/performance_check_collinearity/
library(performance)
check_collinearity(model3)
check_collinearity(model4)


#interpretation
#A VIF less than 5 indicates a low correlation of that predictor with other predictors.
#A value between 5 and 10 indicates a moderate correlation, while VIF values larger than 10 are a sign for high, not tolerable correlation of model predictors.
#The Increased SE column in the output indicates how much larger the standard error is due to the correlation with other predictors.


###### What months have the most effect on prev & intensity? #####

# Model selection calculating AIC - https://www.statology.org/aic-in-r/
######Interpreting output#####
#K: The number of parameters in the model.
#AICc: The AIC value of the model. The lowercase ‘c’ indicates that the AIC has been calculated from the AIC corrected for small sample sizes.
#Delta_AICc: The difference between the AIC of the best model compared to the current model being compared.
#AICcWt: The proportion of the total predictive power that can be found in the model.
#Cum.Wt: The cumulative sum of the AIC weights.
#LL: The log-likelihood of the model. This tells us how likely the model is, given the data we used.

#The model with the lowest AIC value is always listed first. From the output we can see that the following model has the lowest AIC value and is thus the best fitting model

library(AICcmodavg)
MonthlyPrev<- list(model8, model9, model10, model11, model12, model13, model14, model15, model16, model17, model18, model19)
mod.names<- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', "Sept", 'Oct', 'Nov', "Dec")
aictab(cand.set = MonthlyPrev, modnames = mod.names)

### Prevalence ##
model8<- lmer(Prevalence~  WTEMP + SALINITY +(1|oysteryear) + (1|Site)+ (1|MonitoringLocation), Jan)
Anova(model8)
summary(model8)

check_collinearity(model8)

model9<- lmer(Prevalence~ WTEMP +SALINITY+(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Feb)
Anova(model9)
summary(model9)

model10<- lmer(Prevalence~ WTEMP + SALINITY +(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Mar)
Anova(model10)
summary(model10)

model11<- lmer(Prevalence~ WTEMP+ SALINITY +(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Apr)
Anova(model11)
summary(model11)

model12<- lmer(Prevalence~ WTEMP + SALINITY +(1|oysteryear)+ (1|Site) + (1|MonitoringLocation), May)
Anova(model12)
summary(model12)

model13<- lmer(Prevalence~ WTEMP +SALINITY +(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Jun)
Anova(model13)
summary(model13)

model14<- lmer(Prevalence~ WTEMP + SALINITY+(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Jul)
Anova(model14)
summary(model14)

model15<- lmer(Prevalence~ WTEMP + SALINITY+(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Aug)
Anova(model15)
summary(model15)

model16<- lmer(Prevalence~ WTEMP + SALINITY+(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Sept)
Anova(model16)
summary(model16)

model17<- lmer(Prevalence~ WTEMP +SALINITY+(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Oct)
Anova(model17)
summary(model17)

model18<- lmer(Prevalence~ WTEMP + SALINITY +(1|oysteryear)+ (1|Site) + (1|MonitoringLocation), Nov)
Anova(model18)
summary(model18)

model19<- lmer(Prevalence~ WTEMP + SALINITY+(1|oysteryear) + (1|Site) + (1|MonitoringLocation), Dec)
Anova(model19)
summary(model19)

### ADJUSTING P VALUE PREVALENCE & MONTH ###

Ptemp<- c(0.14679, 5.22E-05, 2.53E-14, 0.5421, 0.03704, 0.03449, 6.34E-13, 0.129963, 0.0001182, 0.0358, 0.6128, 0.005463)
PSal<- c(2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 2.00E-16, 5.24E-14, 2.98E-15)
Pint <- c(0.08166, 0.002095, 0.9664, 0.4392, 0.5104, 0.26255, 0.4911, 0.002788, 0.2104471, 0.7682, 0.7702, 0.746582)

p.adjust(Ptemp, method= "fdr")
#[1] 1.761480e-01 2.088000e-04 3.036000e-13 5.913818e-01 5.556000e-02 5.556000e-02 3.804000e-12 1.732840e-01 3.546000e-04
#[10] 5.556000e-02 6.128000e-01 1.311120e-02

p.adjust(PSal, method= "fdr")
#[1] 2.400000e-16 2.400000e-16 2.400000e-16 2.400000e-16 2.400000e-16 2.400000e-16 2.400000e-16 2.400000e-16 2.400000e-16
#[10] 2.400000e-16 5.240000e-14 3.250909e-15

p.adjust(Pint,method="fdr")
# 0.3266400 0.0167280 0.9664000 0.7656000 0.7656000 0.6301200 0.7656000 0.0167280 0.6301200 0.8402182 0.8402182 0.8402182


### Intensity ###

model20<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Jan)
Anova(model20)
summary(model20)

model21<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Feb)
Anova(model21)
summary(model21)

model22<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Mar)
Anova(model22)
summary(model22)

model23<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Apr)
Anova(model23)
summary(model23)

model24<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), May)
Anova(model24)
summary(model24)

model25<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Jun)
Anova(model25)
summary(model25)

model26<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Jul)
Anova(model26)
summary(model26)

model27<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Aug)
Anova(model27)
summary(model27)

model28<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Sept)
Anova(model28)
summary(model28)

model29<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Oct)
Anova(model29)
summary(model29)

model30<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Nov)
Anova(model30)
summary(model30)

model31<- lmer(Mean.Intensity~ WTEMP * SALINITY + (1|Site) + (1|MonitoringLocation), Dec)
Anova(model31)
summary(model31)

## ADJUSTING P VALUE INTENSITY & MONTH ##

ITemp<-c(0.223341,0.0001774,1.05E-11,0.527,0.0003523,0.7495,8.59E-08,0.000972,0.1825,0.6295,0.1978,0.2521)
ISal<-c(1.92E-09,2.35E-08,1.32E-12,2.00E-16,2.20E-16,2.00E-16,2.20E-16,2.20E-16,2.00E-16,3.13E-13,5.86E-11,2.65E-09)
Iint<-c(0.002595,0.0311849,0.2402,0.6954,0.0454286,0.7742,0.09483,0.0194382,0.5565,0.6619,0.3976,0.4582)

p.adjust(ITemp, method="fdr")
#[1] 3.350115e-01 7.096000e-04 1.260000e-10 6.324000e-01 1.056900e-03 7.495000e-01 5.154000e-07 2.332800e-03 3.350115e-01
#[10] 6.867273e-01 3.350115e-01 3.361333e-01

p.adjust(ISal, method="fdr")
#[1] 2.304000e-09 2.350000e-08 1.980000e-12 4.400000e-16 4.400000e-16 4.400000e-16 4.400000e-16 4.400000e-16 4.400000e-16
#[10] 5.365714e-13 7.813333e-11 2.890909e-09

p.adjust(Iint, method="fdr")
#[1] 0.0311400 0.1247396 0.4804000 0.7586182 0.1362858 0.7742000 0.2275920 0.1166292 0.7420000 0.7586182 0.6816000 0.6873000
> 
