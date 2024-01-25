
#Created by: Mariah L. Kachmar
#Date:
#Last updated: 
#Description: Generalized Linear Mixed Effects Model Perkinsus marinus Meta-analysis 

#Set working directory
setwd("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus")

#Load all packaged
library(lme4)
library(readxl)
library(dplyr)
library(ggplot2)
library(glmm)
library(MASS)
library(car)
library(lubridate)

########################### LOAD AND CLEAN DATA FILES ##################################################################



################ DISEASE DATA #######################

##Perkinsus data (Maryland and Virginia)
Perkinsus <- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/PerkinsusMD&VA2.csv",)
View(Perkinsus)

Perk<- na.omit(Perkinsus)
View(Perk)

 ## Removing sites > 0.5 in distance matrix calculations ####
Perk <- filter(Perk, Site != 'RAGGED POINT (LC)', Site != 'PARSONS ISLAND', Site != 'PAGAN (S)' , Site != 'OYSTER SHELL PT. (S)')
View(Perk)

##Intensity individual data from MDDNR- loading and cleaning 
IntensityMD<- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/MDDNR_1990-2020.csv",)
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

Mean_IntensityMD <-na.omit(Mean_IntensityMD)

#Update Maryland sites and remerge maryland and VA data
MD <- Perk %>%
  filter(State == "MD")

VA <- Perk %>%
  filter(State =="VA")

MD_converted <- merge(Mean_IntensityMD, MD)
View(MD_converted)

MD_converted <- MD_converted %>%
  subset(select = -c(Mean.Intensity, SampleDate)) %>%
  rename(Mean.Intensity = Intensity_final)

#Site count
Site_count <- MD_converted %>% 
  distinct(Site) %>% 
  nrow()
Site_count #31

Site_count <- VA %>% 
  distinct(Site) %>% 
  nrow()
Site_count  #38
  
#Recombine VA & MD
  
Perk2 <- rbind(VA, MD_converted)  
View(Perk2)


write.table(Perk2, file="Disease_data_converted.csv", sep=",", row.names=FALSE)





######################### Environmental Data ###############################

EnvALL<- read_excel("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/EnvironmentalData_MD&VAupdated.xlsx")
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




################################## MERGING ENV & PERKINSUS DATA ######################################################
### Merging data sheets using Monthly/yearly means  #### 
Master1<- merge(Perk2, MonthlyMeans, by =c("Year", "MonitoringLocation"))
View(Master1)

Master1<- Master1 %>% 
  rename( "Sample.month"= "Month.x")
View(Master1)

Master1<- Master1 %>% 
  rename( "Month"= "Month.y")
View(Master1)



Merged.data <- merge(MonthlyMeans1, Master1, by =c("Year","Month", "MonitoringLocation", "SampleDate"))
View(Merged.data)

Merged.data$oysteryear=ifelse(Merged.data$Month== "Nov"| Merged.data$Month=="Dec", Merged.data$Year+1, Merged.data$Year)

head(Merged.data)

#Merged.data <- Merged.data %>% 
 # rename( "SampleDate"= "SampleDate.x")

View(Merged.data)

write.table(Merged.data, file="MergedData.csv", sep=",", row.names=FALSE)


## Subset MD ##

Maryland <- Merged.data %>%
  filter(State == "MD")

write.table(Maryland, file = "Maryland_data_all.csv", sep = ",", row.names=FALSE)



############################### STATISTICS ##################################################################


View(Perk)
Perkinsus2 <- Perkinsus[-c(3)]
PM<- na.omit(Perkinsus2)
PM
PM$Prevratio=PM$Prevalence/100

Perk2$Year <- as.numeric(Perk$Year)
Perk2$Lat <- as.numeric(Perk$Lat)
Perk2$Prevalence <- as.numeric(Perk$Prevalence)

Perk2$oysteryear=ifelse(Perk2$Month== "Nov"| Perk2$Month=="Dec", Perk2$Year+1, Perk2$Year)
head(Perk2)



### using lmer(), polr(), glmm()


### spatio-temporal trends Perkinsus ###


model1<- glmm(fixed = Prevalence ~ Region * oysteryear , random = list(Site = ~1), data = Perk2, family.glmm = binomial.glmm, varcomps.names = c("Site"))
Anova(model1)

Perk2$Mean.Intensity <- as.factor(Perk2$Mean.Intensity)
model2<- polr(Mean.Intensity ~ Region * oysteryear , data = Perk2, Hess = TRUE)
Anova(model2)

modelX<-glmm(fixed =Prevalence ~ Site * oysteryear, random = list(Region = ~10), data= Perk2, family.glmm = binomial.glmm, varcomps.names = c("Region"))
Anova(modelX)

modelY<- polr(Mean.Intensity ~ Site * oysteryear, data = Perk2, Hess = TRUE)
Anova(modelY)

### Environmental Data trends ####

Merged.data$SampleDate<- scale(Merged.data$SampleDate.y)
Merged.data$Year<- scale(Merged.data$Year)
Merged.data$Lat<- scale(Merged.data$Lat)



model5<- lmer(WTEMP ~  Year * Lat + (1|Site), data = Merged.data)
Anova(model5)
  
model6<- lmer(SALINITY ~ Year * Lat + (1|Site), data = Merged.data)
Anova(model6)



### Pearson's Correlation- Temperature and Salinity ### https://www.r-bloggers.com/2021/10/pearson-correlation-in-r/
#Correlation<- cor(Merged.data$WTEMP, Merged.data$SALINITY, method= 'pearson')
#Correlation
#[1] 0.06595104
# r value of 0-0.3 = not correlated


##### Environmental effects on Perkinsus prev & intensity ########



model3<- glmm(fixed = Prevalence ~ oysteryear+ WTEMP +SALINITY+ Region , random =list(Site = ~1, MonitoringLocation = ~1), 
              Merged.data, family.glmm = binomial.glmm, 
              varcomps.names = c("Site", "MonitoringLocation"))
Anova(model3)
summary(model3)

Merged.data$Mean.Intensity<-as.factor(Merged.data$Mean.Intensity)
model4<- polr(Mean.Intensity~ oysteryear+ WTEMP +SALINITY+ Region , Merged.data, Hess = TRUE)
Anova(model4)
summary(model4)


### Collinearity- https://www.codingprof.com/3-ways-to-test-for-multicollinearity-in-r-examples/ ###
#library("olsrr") ## error - using wrong model, need lm()
#library("corrplot") ## need to be numeric
#corrplot(cor(Merged.data), method = "number")

#https://easystats.github.io/blog/posts/performance_check_collinearity/
#library(performance)
#check_collinearity(model3)
#check_collinearity(model4)



#interpretation
#A VIF less than 5 indicates a low correlation of that predictor with other predictors.
#A value between 5 and 10 indicates a moderate correlation, while VIF values larger than 10 are a sign for high, not tolerable correlation of model predictors.
#The Increased SE column in the output indicates how much larger the standard error is due to the correlation with other predictors.

########################### Monthly Models #############################################
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

#library(AICcmodavg)
#MonthlyPrev<- list(model8, model9, model10, model11, model12, model13, model14, model15, model16, model17, model18, model19)
#mod.names<- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', "Sept", 'Oct', 'Nov', "Dec")
#aictab(cand.set = MonthlyPrev, modnames = mod.names)


### Prevalence ##
model8<- lmer(Prevalence~ WTEMP + SALINITY+ Region +(1|Day) + (1|Site)+ (1|MonitoringLocation), Jan)
Anova(model8)
summary(model8)

check_collinearity(model8)

model9<- lmer(Prevalence~ WTEMP +SALINITY+ Region+(1|Day) + (1|Site) + (1|MonitoringLocation), Feb)
Anova(model9)
summary(model9)

model10<- lmer(Prevalence~ WTEMP + SALINITY +Region+(1|Day)+ (1|Site) + (1|MonitoringLocation), Mar)
Anova(model10)
summary(model10)

model11<- lmer(Prevalence~ WTEMP+ SALINITY +Region +(1|Day)+ (1|Site) + (1|MonitoringLocation), Apr)
Anova(model11)
summary(model11)

model12<- lmer(Prevalence~ WTEMP + SALINITY +Region+(1|Day)+ (1|Site) + (1|MonitoringLocation), May)
Anova(model12)
summary(model12)

model13<- lmer(Prevalence~ WTEMP +SALINITY +Region+(1|Day)+ (1|Site) + (1|MonitoringLocation), Jun)
Anova(model13)
summary(model13)

model14<- lmer(Prevalence~ WTEMP + SALINITY+ Region+(1|Day) + (1|Site) + (1|MonitoringLocation), Jul)
Anova(model14)
summary(model14)

model15<- lmer(Prevalence~ WTEMP + SALINITY+Region +(1|Day) + (1|Site) + (1|MonitoringLocation), Aug)
Anova(model15)
summary(model15)

model16<- lmer(Prevalence~ WTEMP + SALINITY+Region+(1|Day)  + (1|Site) + (1|MonitoringLocation), Sept)
Anova(model16)
summary(model16)

model17<- lmer(Prevalence~ WTEMP +SALINITY+Region+(1|Day) + (1|Site) + (1|MonitoringLocation), Oct)
Anova(model17)
summary(model17)

model18<- lmer(Prevalence~ WTEMP + SALINITY +Region+(1|Day)+(1|Site) + (1|MonitoringLocation), Nov)
Anova(model18)
summary(model18)

model19<- lmer(Prevalence~ WTEMP + SALINITY+Region+(1|Day) + (1|Site) + (1|MonitoringLocation), Dec)
Anova(model19)
summary(model19)

### ADJUSTING P VALUE PREVALENCE & MONTH ###

Ptemp<- c(0.7865,6.45E-05,6.97E-09,0.001132, 0.2381 ,0.65623,8.30E-06,0.59027,1.12E-05,4.53E-05,0.7505,0.004616)

PSal<- c(1.56E-11,1.25E-12,2.20E-16,7.81E-15,2.00E-16,1.76E-11,2.20E-16,2.00E-16,2.20E-16,2.20E-16,1.26E-09,1.74E-10)
PReg <- c(0.1071
          ,0.159
          ,0.1907
          ,0.102586
          ,0.1198
          ,0.06833
          ,0.1414
          ,0.07093
          ,0.1798
          ,0.1287
          ,0.2073
          ,0.198185)

n.ptemp<-p.adjust(Ptemp, method= "fdr")
#[1] 7.865000e-01 1.548000e-04 8.364000e-08 2.264000e-03 3.571500e-01 7.865000e-01 4.480000e-05 7.865000e-01 4.480000e-05
#[10] 1.359000e-04 7.865000e-01 7.913143e-03
View(as.table(n.ptemp))

n.psal <-p.adjust(PSal, method= "fdr")
#[1] 2.080000e-11 1.875000e-12 4.400000e-16 1.338857e-14 4.400000e-16 2.112000e-11 4.400000e-16 4.400000e-16 4.400000e-16
#[10] 4.400000e-16 1.260000e-09 1.898182e-10
View(as.table(n.psal))

n.preg<-p.adjust(PReg,method="fdr")
#  [1] 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073 0.2073
View(as.table(n.preg))

################################ Intensity ######################

model20<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Jan, Hess = TRUE)
Anova(model20)
summary(model20)

model21<- polr(Mean.Intensity~ WTEMP+ SALINITY +Region, Feb, Hess= TRUE)
Anova(model21)
summary(model21)

model22<- polr(Mean.Intensity~ WTEMP +SALINITY +Region, Mar, Hess= TRUE)
Anova(model22)
summary(model22)

model23<-polr(Mean.Intensity~ WTEMP + SALINITY +Region, Apr, Hess= TRUE)
Anova(model23)
summary(model23)

model24<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, May, Hess= TRUE)
Anova(model24)
summary(model24)

model25<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Jun, Hess= TRUE)
Anova(model25)
summary(model25)

model26<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Jul, Hess= TRUE)
Anova(model26)
summary(model26)

model27<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Aug, Hess= TRUE)
Anova(model27)
summary(model27)

model28<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Sept, Hess= TRUE)
Anova(model28)
summary(model28)

model29<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Oct, Hess= TRUE)
Anova(model29)
summary(model29)

model30<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Nov, Hess= TRUE)
Anova(model30)
summary(model30)

model31<- polr(Mean.Intensity~ WTEMP + SALINITY +Region, Dec, Hess= TRUE)
Anova(model31)
summary(model31)

## ADJUSTING P VALUE INTENSITY & MONTH ##

ITemp<-c(0.32431
         ,0.138779
         ,5.77E-07
         ,4.43E-05
         ,0.006992
         ,0.06077
         ,0.5943
         ,0.1668
         ,0.01139
         ,0.2461
         ,0.183275
         ,0.362107)
ISal<-c(0.02944
        ,0.004285
        ,0.0001118
        ,0.0003577
        ,2.20E-16
        ,2.24E-08
        ,4.57E-11
        ,1.94E-08
        ,3.51E-06
        ,2.79E-05
        ,0.004623
        ,0.0006841)
IReg<-c(0.87721
        ,0.812266
        ,0.717824
        ,0.8082627
        ,2.20E-16
        ,0.86188
        ,0.6702
        ,0.7002
        ,0.78725
        ,0.7669
        ,0.791898
        ,0.8413955)

n.itemp<-p.adjust(ITemp, method="fdr")
View(as.table(n.itemp))

n.isal<-p.adjust(ISal, method="fdr")
View(as.table(n.isal))

n.ireg<-p.adjust(IReg, method="fdr")
View(as.table(n.ireg))


