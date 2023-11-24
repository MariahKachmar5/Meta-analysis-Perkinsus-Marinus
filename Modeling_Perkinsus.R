
#Created by: Mariah L. Kachmar
#Date:
#Last updated: 
#Description: Generalized Linear Mixed Effects Model Perkinsus marinus Meta-analysis 

#Set working directory
setwd("~/Documents/UMBC/Meta-Analysis")

#Load all packaged
library(lme4)
library(readxl)
library(dplyr)

####################### LOAD AND CLEAN DATA FILES ####################################

################ DISEASE DATA #######################

##Perkinsus data (Maryland and Virginia)
Perkinsus <- read.csv("~/Documents/UMBC/Meta-Analysis/PerkinsusMD&VA2.csv",)
View(Perkinsus)

Perk<- na.omit(Perkinsus)
View(Perk)

 ## Removing sites > 0.5 in distance matrix calculations ####
Perk <- filter(Perk, Site != 'RAGGED POINT (LC)', Site != 'PARSONS ISLAND', Site != 'PAGAN (S)' , Site != 'OYSTER SHELL PT. (S)')
View(Perk)

##Intensity individual data from MDDNR- loading and cleaning 
IntensityMD<- read.csv("~/Documents/UMBC/Meta-Analysis/MDDNR_1990-2020.csv",)
View(IntensityMD)

as.data.frame(IntensityMD)

#Rebinning intensity scores to follow VIMS

IntensityMD<- IntensityMD %>%
  mutate(Intensity_score = recode(Intensity, 
                                 "0" = 0, "1" = 0.5, "2" = 3, "3"= 3, "4"= 3, "5"= 5,"6"= 5, "7"=5))
head(IntensityMD)

View(IntensityMD)
#Calculating mean intensity for each site and year
IntensityMD$prevalent = ifelse(IntensityMD$Intensity_score == 0, '0',
                            ifelse(IntensityMD$Intensity_score > 0, '1', NA))

present <- nrow(IntensityMD %>% filter(prevalent == '1'))
absent <- nrow(IntensityMD %>% filter(prevalent == '0'))

prevalence_all <- present/(present+absent)
prevalence_all

str(IntensityMD$prevalent)
IntensityMD$prevalent <- as.numeric(IntensityMD$prevalent)

Mean_IntensityMD <- IntensityMD %>%
  group_by(Region, Site, Year, SampleDate) %>%
  summarize(Intensity_final = sum(Intensity_score = as.numeric(as.character(Intensity_score)), 
                            na.rm = TRUE)/sum(prevalent, na.rm=TRUE))

View(Mean_IntensityMD) # Need to fix Na values 


######################### Environmental Data ##############################

EnvALL<- read_excel("~/Documents/UMBC/Meta-Analysis/EnvironmentalData_MD&VAupdated.xlsx")
View(EnvALL)
write.table(Env, file="Env.csv", sep=",", row.names=FALSE)

EnvALL<- filter(EnvALL, SampleDate > '1989-12-31', Layer == "S")
View(EnvALL)

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

EnvALL$Day <-day(EnvALL$SampleDate)
View(EnvALL)


### add month column to data ###
#Env1 <- Env
#nv1["Month"] <- Env$Month
#Env1
#View(Env1)

#Env1 <- Env
#Env1["Year"] <- Env$Year
#Env1
#View(Env1)

### subsetting environmental data by parameter ###

PH <- EnvALL[EnvALL$Parameter == "PH",]
View(PH)
PH <- PH %>%
  rename(pH = MeasureValue)

SALINITY <- EnvALL[EnvALL$Parameter == "SALINITY",]
SALINITY
SALINITY <- SALINITY %>%
  rename(SALINITY = MeasureValue)
View(SALINITY)

WTEMP <-EnvALL[EnvALL$Parameter == "WTEMP",]
View(WTEMP)
WTEMP <- WTEMP %>%
  rename(WTEMP = MeasureValue)

### REMERGING ENVIRONMENTAL DATA ####

MasterENV<- merge(SALINITY, WTEMP, by =c("Year","Month", "Day", "MonitoringLocation", "SampleDate", "Latitude", "Longitude", "Depth", "Layer"), all = TRUE)
View(MasterENV)

MasterENV<- subset(MasterENV, select = -c(Parameter.x,Parameter.y))

MasterENV<-na.omit(MasterENV)
View(MasterENV)

### Filtering environmental data so there is only one sample date per month each year ###

library(lubridate)
library(dplyr)

class(MasterENV$SampleDate)
MasterENV$SampleDate <- as.Date(MasterENV$SampleDate, format = "%Y-%m-%d")

MasterENV$Month_Year <- format(MasterENV$SampleDate, "%Y-%m")
MasterENV$Month_Day <- format(MasterENV$SampleDate, "%m-%d")


MasterENV_filtered3<- MasterENV %>%
  group_by(MonitoringLocation,Year, Month) %>%
  slice(1)%>%
  ungroup()
View(MasterENV_filtered3)

##Alternative way ###
#MasterENV <- MasterENV %>%
#  mutate(month_year_monitoringlocation = paste0(format(SampleDate, "%Y-%m"), "-", MonitoringLocation))
#View(MasterENV)

#MasterENV_filtered <- MasterENV %>%
 # group_by(month_year_monitoringlocation) %>%
 # filter(n()==1) %>%
  #ungroup()


envplot<- ggplot(MasterENV_filtered3, aes(Year, SALINITY, color = MonitoringLocation))+geom_smooth(se= FALSE)
envplot

# removing sites with missing data from 1990-2020
desired_start_year <- 1990 
site_start_years <- MasterENV_filtered3 %>%
  group_by(MonitoringLocation) %>%
  summarize(start_year = min(year(SampleDate)))

MasterENV_filtered2<- MasterENV_filtered3 %>%
  inner_join(site_start_years, by= "MonitoringLocation") %>%
  filter(start_year == desired_start_year) %>%
  select(-start_year)
View(MasterENV_filtered2)

### Yearly means for environmental parameters by site and time ###

library(dplyr)
Smeans <-MasterENV_filtered2 %>%
  group_by(Year, Month, SALINITY, MonitoringLocation) %>%
  summarize(SALINITY = mean(SALINITY))
View(Smeans)

#### TEMP ###
MonthlyMeans<-MasterENV_filtered2 %>%
  group_by(Month, Year, WTEMP, MonitoringLocation, SampleDate) %>%
  summarize(WTEMP = mean(WTEMP))
View(MonthlyMeans)

MonthlyMeans$Month <- month.abb[MonthlyMeans$Month]

head(MonthlyMeans)
View(MonthlyMeans)

### Salinity ###

MonthlyMeans1<-MasterENV_filtered2 %>%
  group_by(Month, Year, SALINITY, MonitoringLocation, SampleDate) %>%
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

Merged.data <- Merged.data %>% 
  rename( "SampleDate"= "SampleDate.x")

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

### spatio-temporal trends Perkinsus ###


model1<- lmer(Prevalence ~ Lat * oysteryear + (1|Site), data = Perk)
Anova(model1)


model2<- lmer(Mean.Intensity ~ Lat * oysteryear + (1|Site), data = Perk)
Anova(model2)

modelX<-lmer(Prevalence ~ Site * oysteryear + (1|Lat), data= Perk)
Anova(modelX)

modelY<- lmer(Mean.Intensity ~ Site * oysteryear + (1|Lat), data = Perk)
Anova(modelY)

### Environmental Data trends ####

Merged.data$SampleDate<- scale(Merged.data$SampleDate)
Merged.data$Year<- scale(Merged.data$Year)
Merged.data$Lat<- scale(Merged.data$Lat)

View(Merged.data)
Merged.data$Day<- format(Merged.data$SampleDate, "%d")
head(Merged.data)

model5<- lmer(WTEMP ~  Year * Day * Month + (1|Site), data = Merged.data)
Anova(model5)
  
model6<- lmer(SALINITY ~ Lat * Year *Day + (1|Site), data = Merged.data)
Anova(model6)

View(Merged.data)

Month_day<- table(Merged.data$Month, Merged.data$Day)
View(Month_day)

### Pearson's Correlation- Temperature and Salinity ### https://www.r-bloggers.com/2021/10/pearson-correlation-in-r/
Correlation<- cor(Merged.data$WTEMP, Merged.data$SALINITY, method= 'pearson')
Correlation
#[1] 0.06595104
# r value of 0-0.3 = not correlated


##### Environmental effects on Perkinsus prev & intensity ########

##Quartiles ###

Merged.data<-na.omit(Merged.data)

quantile_Temp <- Merged.data %>%
  group_by(Site, Year) %>%
  summarize(
    T_Q10 = quantile(WTEMP, probs = 0.1),
    T_Q90 = quantile(WTEMP, probs = 0.9)
  ) %>%
  ungroup()
View(quantile_Temp)

quantile_Sal <- Merged.data %>%
  group_by(Site, Year) %>%
  summarize(
    S_Q10 = quantile(SALINITY, probs = 0.1),
    S_Q90 = quantile(SALINITY, probs = 0.9)
  ) %>%
  ungroup()
View(quantile_Sal)

Merged.data1 <- merge(Merged.data, quantile_Temp, by =c("Year","Site"))
View(Merged.data1)

Merged.data_Q <- merge(Merged.data1, quantile_Sal, by = c("Year","Site"))
View(Merged.data_Q)

#Temp_90<- quantile(MasterENV$WTEMP, probs=c(0.9))
#Temp_10<- quantile(MasterENV$WTEMP, probs=c(0.1))

#Sal_90<- quantile(MasterENV$SALINITY,probs=c(0.9))
#Sal_10<- quantile(MasterENV$WTEMP, probs=c(0.1))

#Merged.data_Q <- cbind(Merged.data, Temp_90, Temp_10, Sal_90,Sal_10)
#View(Merged.data_Q)

library(car)

class(Merged.data_Q$Prevalence)
Merged.data_Q$Prevalence <- as.numeric(Merged.data_Q$Prevalence)
Merged.data_Q$oysteryear <- as.numeric(Merged.data_Q$oysteryear)
Merged.data_Q$WTEMP <- as.numeric(Merged.data_Q$WTEMP)
Merged.data_Q$SALINITY <- as.numeric(Merged.data_Q$SALINITY)
Merged.data_Q$T_Q10 <- as.numeric(Merged.data_Q$T_Q10)
Merged.data_Q$T_Q90 <- as.numeric(Merged.data_Q$T_Q90)
Merged.data_Q$S_Q10 <- as.numeric(Merged.data_Q$S_Q10)
Merged.data_Q$S_Q90 <- as.numeric(Merged.data_Q$S_910)

View(Merged.data_Q$T_Q90)
View(Merged.data_Q)

class(Merged.data$Site)
View(Merged.data$Site)

View(Merged.data)

model3<- lmer(Prevalence~ oysteryear+ T_Q90+T_Q10 + S_Q90+ S_Q10+ Region +  (1|Site) + (1|MonitoringLocation),  Merged.data_Q)
Anova(model3)
summary(model3)
#Effect sizes: 
# Temperature d = 25.66 / 901.35 = .03
# Salinity d = 33.99 / 901.35 = .04

model4<- lmer(Mean.Intensity~ oysteryear* T_Q90+T_Q10 + S_Q90+ S_Q10*Region + (1|Site) + (1|MonitoringLocation), Merged.data)
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
model8<- lmer(Prevalence~ WTEMP + SALINITY+ Region  + (1|Site)+ (1|MonitoringLocation), Jan)
Anova(model8)
summary(model8)

check_collinearity(model8)

model9<- lmer(Prevalence~ WTEMP +SALINITY+ Region + (1|Site) + (1|MonitoringLocation), Feb)
Anova(model9)
summary(model9)

model10<- lmer(Prevalence~ WTEMP + SALINITY +Region+ (1|Site) + (1|MonitoringLocation), Mar)
Anova(model10)
summary(model10)

model11<- lmer(Prevalence~ WTEMP+ SALINITY +Region + (1|Site) + (1|MonitoringLocation), Apr)
Anova(model11)
summary(model11)

model12<- lmer(Prevalence~ WTEMP + SALINITY +Region+ (1|Site) + (1|MonitoringLocation), May)
Anova(model12)
summary(model12)

model13<- lmer(Prevalence~ WTEMP +SALINITY +Region+ (1|oysteryear) + (1|Site) + (1|MonitoringLocation), Jun)
Anova(model13)
summary(model13)

model14<- lmer(Prevalence~ WTEMP + SALINITY+ Region + (1|oysteryear) + (1|Site) + (1|MonitoringLocation), Jul)
Anova(model14)
summary(model14)

model15<- lmer(Prevalence~ WTEMP + SALINITY+Region + (1|oysteryear)+ (1|Site) + (1|MonitoringLocation), Aug)
Anova(model15)
summary(model15)

model16<- lmer(Prevalence~ WTEMP + SALINITY+Region + (1|oysteryear)+ (1|Site) + (1|MonitoringLocation), Sept)
Anova(model16)
summary(model16)

model17<- lmer(Prevalence~ WTEMP +SALINITY+Region+ (1|oysteryear) + (1|Site) + (1|MonitoringLocation), Oct)
Anova(model17)
summary(model17)

model18<- lmer(Prevalence~ WTEMP + SALINITY +Region+ (1|oysteryear) +(1|Site) + (1|MonitoringLocation), Nov)
Anova(model18)
summary(model18)

model19<- lmer(Prevalence~ WTEMP + SALINITY+Region+ (1|oysteryear) + (1|Site) + (1|MonitoringLocation), Dec)
Anova(model19)
summary(model19)

#### Pearson's correlation between models ###
#Predicted values#
Predicted_Jan<- predict(model8, Jan)
View(Predicted_Jan)
Predicted_Feb<- predict(model9, Feb)
Predicted_Mar<- predict(model10, Mar)
Predicted_Apr<- predict(model11, Apr)
Predicted_May<- predict(model12, May)
Predicted_Jun<- predict(model13, Jun)
Predicted_Jul<- predict(model14, Jul)
Predicted_Aug<- predict(model15, Aug)
Predicted_Sept<- predict(model16, Sept)
Predicted_Oct<- predict(model17, Oct)
Predicted_Nov<- predict(model18, Nov)
Predicted_Dec<- predict(model19, Dec)

Predicted_values<-cbind(Jan= Predicted_Jan, Feb= Predicted_Feb, Mar= Predicted_Mar, Apr= Predicted_Apr,
                         May= Predicted_May, Jun= Predicted_Jun, Jul= Predicted_Jul,Aug= Predicted_Aug,
                         Sept= Predicted_Sept, Oct=Predicted_Oct, Nov= Predicted_Nov, Dec= Predicted_Dec)
View(Predicted_values)
Predicted_values<-na.omit(Predicted_values)
Model_correlations <-cor(Predicted_values)
print(Model_correlations)

#######  Jan        Feb        Mar        Apr        May        Jun
#Jan  1.00000000 0.21016485 0.05728583 0.04698284 0.03515336 0.01108076
#Feb  0.21016485 1.00000000 0.08397097 0.03502557 0.06677192 0.05700673
#Mar  0.05728583 0.08397097 1.00000000 0.31743044 0.30746188 0.33492442
#Apr  0.04698284 0.03502557 0.31743044 1.00000000 0.42750554 0.40434910
#May  0.03515336 0.06677192 0.30746188 0.42750554 1.00000000 0.48093075
#Jun  0.01108076 0.05700673 0.33492442 0.40434910 0.48093075 1.00000000
#Jul  0.01946920 0.04895789 0.32710932 0.37211123 0.41575871 0.46842519
#Aug  0.05931225 0.06574116 0.29731629 0.47853431 0.41970089 0.39318013
#Sept 0.04843564 0.05661391 0.34545181 0.28909222 0.29289213 0.24506444
#Oct  0.07023044 0.02360382 0.33610318 0.38694286 0.44032090 0.41329568
#Nov  0.12194878 0.12610892 0.10561802 0.05815338 0.10956805 0.10881210
#Dec  0.19051811 0.23474141 0.06621542 0.07005585 0.05407221 0.06586653
#.        Jul        Aug       Sept        Oct        Nov        Dec
#Jan  0.01946920 0.05931225 0.04843564 0.07023044 0.12194878 0.19051811
#Feb  0.04895789 0.06574116 0.05661391 0.02360382 0.12610892 0.23474141
#Mar  0.32710932 0.29731629 0.34545181 0.33610318 0.10561802 0.06621542
#Apr  0.37211123 0.47853431 0.28909222 0.38694286 0.05815338 0.07005585
#May  0.41575871 0.41970089 0.29289213 0.44032090 0.10956805 0.05407221
#Jun  0.46842519 0.39318013 0.24506444 0.41329568 0.10881210 0.06586653
#Jul  1.00000000 0.39551679 0.26866410 0.40057943 0.11259983 0.05139048
#Aug  0.39551679 1.00000000 0.27430900 0.35177701 0.08203247 0.08956273
#Sept 0.26866410 0.27430900 1.00000000 0.29249666 0.08480751 0.08633808
#Oct  0.40057943 0.35177701 0.29249666 1.00000000 0.09810091 0.03906420
#Nov  0.11259983 0.08203247 0.08480751 0.09810091 1.00000000 0.24345313
#Dec  0.05139048 0.08956273 0.08633808 0.03906420 0.24345313 1.00000000

#r=0; there is no relation between the variable.
#r=+1; perfectly positively correlated.
#r=-1; perfectly negatively correlated.
#r= 0 to 0.30; negligible correlation.
#r=0.30 to 0.50; moderate correlation.
#r=0.50 to 1 highly correlated.

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
