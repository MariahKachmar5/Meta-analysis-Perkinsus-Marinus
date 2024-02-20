
#Created by: Mariah L. Kachmar
#Date:
#Last updated: 1/25/24
#Description: Generalized Linear Mixed Effects Model Perkinsus marinus Meta-analysis 

#NOTES: receiving error for glmm() for prevalence data

#####################################################################################################

#Set working directory
setwd("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/")

#Load all packaged
library(lme4)
library(readxl)
library(dplyr)
library(ggplot2)
library(glmm)
library(MASS)
library(car)
library(lubridate)
library(glmmTMB)
library(ordinal)
library(knitr)
library(broom)

########################### LOAD AND CLEAN DATA FILES ##################################################################



################ DISEASE DATA #######################

##Perkinsus data (Maryland and Virginia)
Perkinsus <- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/PerkinsusMD&VA2.csv",)
View(Perkinsus)

Perk<- na.omit(Perkinsus)
View(Perk)

 ## Removing sites > 0.5 in distance matrix calculations ####
#Perk <- filter(Perk, Site != 'RAGGED POINT (LC)', Site != 'PARSONS ISLAND', Site != 'PAGAN (S)' , Site != 'OYSTER SHELL PT. (S)')


##Intensity individual data from MDDNR- loading and cleaning 
IntensityMD<- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/MDDNR_1990-2020.csv",)

as.data.frame(IntensityMD)

#Rebinning intensity scores to follow VIMS

IntensityMD<- IntensityMD %>%
  mutate(Intensity_score = dplyr::recode(Intensity, 
                                 "0" = 0, "1" = 0.5, "2" = 3, "3"= 3, "4"= 3, "5"= 5,"6"= 5, "7"=5))
head(IntensityMD)
IntensityMD <-na.omit(IntensityMD)
View(IntensityMD)
#Calculating mean intensity for each site and year
IntensityMD$prevalent = ifelse(IntensityMD$Intensity_score == 0, '0',
                            ifelse(IntensityMD$Intensity_score > 0, '1', NA))

present <- nrow(IntensityMD %>% filter(prevalent == '1'))
absent <- nrow(IntensityMD %>% filter(prevalent == '0'))

prevalence_all <- present/(present+absent)
prevalence_all

View(IntensityMD)

str(IntensityMD$prevalent)
IntensityMD$prevalent <- as.numeric(IntensityMD$prevalent)

Mean_IntensityMD <- IntensityMD %>%
  dplyr::group_by(Region, Site, Year, SampleDate) %>%
  dplyr::summarize(Intensity_final = sum(Intensity_score = as.numeric(as.character(Intensity_score)), 
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
  dplyr::rename(Mean.Intensity = Intensity_final)

#Site count
Site_count <- MD_converted %>% 
  distinct(Site) %>% 
  nrow()
Site_count #36

Site_count <- VA %>% 
  distinct(Site) %>% 
  nrow()
Site_count  #38
  
#Recombine VA & MD
  
Perk2 <- rbind(VA, MD_converted)  
View(Perk2)


write.table(Perk2, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Disease_data_converted.csv", sep=",", row.names=FALSE)





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

write.table(Maryland, file = "~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Filtered_Env_Master.csv", sep = ",", row.names=FALSE)


### Yearly means for environmental parameters by site and time ###


Smeans <-MasterENV_filtered2 %>%
  group_by(Year, Month, SALINITY, MonitoringLocation) %>%
  summarize(SALINITY = mean(SALINITY))
View(Smeans)

#### TEMP ###
MonthlyMeans<-MasterENV_filtered2 %>%
  group_by(Month, Year, WTEMP, MonitoringLocation, SampleDate, Latitude, Longitude) %>%
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

View(Perk2)
Master1<- Master1 %>% 
  dplyr::rename("Sample.month"= "Month.x")
View(Master1)

Master1<- Master1 %>% 
  dplyr::rename( "Month"= "Month.y")
View(Master1)


Merged.data <- merge(MonthlyMeans1, Master1, by =c("Year","Month", "MonitoringLocation", "SampleDate"))
View(Merged.data)

Merged.data$oysteryear=ifelse(Merged.data$Month== "Nov"| Merged.data$Month=="Dec", Merged.data$Year+1, Merged.data$Year)

head(Merged.data)

#Merged.data <- Merged.data %>% 
 # rename( "SampleDate"= "SampleDate.x")

View(Merged.data)

Merged.data<- Merged.data %>% 
  dplyr::rename( "Lat_Env"= "Latitude")

Merged.data<- Merged.data %>% 
  dplyr::rename( "Long_Env"= "Longitude")
View(Merged.data)

write.table(Merged.data, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/MergedData.csv", sep=",", row.names=FALSE)


## Subset MD for Sarah ##

Maryland <- Merged.data %>%
  filter(State == "MD")
View(Maryland)
write.table(Maryland, file = "~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Maryland_data_all.csv", sep = ",", row.names=FALSE)



############################### STATISTICS ##################################################################

# Create ratios for beta distribution
View(Perk2)

Perk2$Prevalence <- as.numeric(Perk2$Prevalence)

Perk2$Prev_ratio <- Perk2$Prevalence/100
head(Perk2)
View(Perk2)

#Perk2$Year <- as.numeric(Perk$Year)
#Perk2$Lat <- as.numeric(Perk$Lat)
#Perk2$Prevalence <- as.numeric(Perk$Prevalence)

#Oyster year
Perk2$oysteryear=ifelse(Perk2$Month== "Nov"| Perk2$Month=="Dec", Perk2$Year+1, Perk2$Year)
head(Perk2)

Perk2<-na.omit(Perk2)

View(Perk2)
### using lmer(), polr(), glmm()


### spatio-temporal trends Perkinsus ###


epsilon <- 1e-6  # Small constant to avoid exactly 1
Perk2$Prev_ep <- pmin(pmax(Perk2$Prev_ratio, epsilon), 1 - epsilon)

#Region
model1<- glmmTMB(Prev_ep ~ Region + oysteryear + (1|Site), data = Perk2, family = beta_family())
Anova(model1)

Perk2$Mean.Intensity <- as.factor(Perk2$Mean.Intensity)
model2<- lmer(Mean.Intensity ~ Region + oysteryear + (1|Site) , data = Perk2)
Anova(model2)

#Site
modelX<-glmmTMB(Prev_ep ~ Site + oysteryear +(1|Region) , data= Perk2, family = beta_family())
Anova(modelX)

modelY<- lmer(Mean.Intensity ~ Site + oysteryear+ (1|Region), data = Perk2)
Anova(modelY)

### Environmental Data trends ####

Merged.data$SampleDate<- scale(Merged.data$SampleDate.y)
Merged.data$Year<- scale(Merged.data$Year)
Merged.data$Lat<- scale(Merged.data$Lat)



model5<- lmer(WTEMP ~  Year + Region + (1|Site), data = Merged.data)
Anova(model5)
  
model6<- lmer(SALINITY ~ Year + Region + (1|Site), data = Merged.data)
Anova(model6)



### Pearson's Correlation- Temperature and Salinity ### https://www.r-bloggers.com/2021/10/pearson-correlation-in-r/
#Correlation<- cor(Merged.data$WTEMP, Merged.data$SALINITY, method= 'pearson')
#Correlation
#[1] 0.06595104
# r value of 0-0.3 = not correlated


##### Environmental effects on Perkinsus prev & intensity ########
#Epsilon ratio calculation
Merged.data$Prevalence <- as.numeric(Merged.data$Prevalence)

Merged.data$Prev_ratio <- Merged.data$Prevalence/100
head(Merged.data)

Merged.data$Prev_ep <- pmin(pmax(Merged.data$Prev_ratio, epsilon), 1 - epsilon)
head(Merged.data)

#models 
model3<- glmmTMB(Prev_ep ~  oysteryear + WTEMP + SALINITY + Region + (1|Site), data = Merged.data, family = beta_family())
Anova(model3)
summary(model3)

model4<- lmer(Mean.Intensity~ oysteryear+ WTEMP +SALINITY+ Region +(1|Site) , data= Merged.data)
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
model8<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Jan, family = beta_family())
results8<-tidy(Anova(model8))
summary(model8)


model9<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Feb, family = beta_family())
results9<-tidy(Anova(model9))
summary(model9)

model10<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Mar, family = beta_family())
results10<-tidy(Anova(model10))
summary(model10)

model11<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Apr, family = beta_family())
results11<-tidy(Anova(model11))
summary(model11)

model12<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), May, family = beta_family())
results12<-tidy(Anova(model12))
summary(model12)

model13<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Jun, family = beta_family())
results13<-tidy(Anova(model13))
summary(model13)

model14<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Jul, family = beta_family())
results14<-tidy(Anova(model14))
summary(model14)

model15<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Aug, family = beta_family())
results15<-tidy(Anova(model15))
summary(model15)

model16<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Sept, family = beta_family())
results16<-tidy(Anova(model16))
summary(model16)

model17<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Oct, family = beta_family())
results17<-tidy(Anova(model17))
summary(model17)

model18<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Nov, family = beta_family())
results18<-tidy(Anova(model18))
summary(model18)

model19<- glmmTMB(Prev_ep~ WTEMP + SALINITY+ Region + (1|Site)+ (1|MonitoringLocation), Dec, family = beta_family())
results19<-tidy(Anova(model19))
summary(model19)

## Combining Anova () chi sq stats into a table 
Monthly_Prevalence_Results <-rbind(results8, results9, results10, results11, results12, results13, results14, results15, results16, results17, results18, results19)
View(Monthly_Prevalence_Results)

### ADJUSTING P VALUE PREVALENCE & MONTH ###

Monthly_Prevalence_Results$fdr.p.value <- p.adjust(Monthly_Prevalence_Results$p.value, method = "fdr")
Monthly_Prevalence_Results
Monthly_Prevalence_Results$Month <- c("Jan", "Jan", "Jan", "Feb", "Feb", "Feb", "Mar", "Mar", "Mar", "Apr", "Apr","Apr", "May", "May", "May", "Jun", "Jun", "Jun", "Jul", "Jul","Jul", 
                                      "Aug", "Aug", "Aug", "Sept", "Sept", "Sept", "Oct", "Oct", "Oct", "Nov", "Nov", "Nov", "Dec", "Dec", "Dec")
write.table(Monthly_Prevalence_Results, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Monthly_Prevalence_Results.csv", sep=",", row.names=FALSE)


################################ Intensity ######################

model20<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region + (1|Site) + (1|MonitoringLocation), data = Jan, Hess = TRUE)
Anova(model20)
results20<- tidy(Anova(model20))
summary(model20)

model21<- lmer(Mean.Intensity~ WTEMP+ SALINITY +Region +(1|Site) + (1|MonitoringLocation), Feb)
results21<-tidy(Anova(model21))
summary(model21)

model22<- lmer(Mean.Intensity~ WTEMP +SALINITY +Region+(1|Site) + (1|MonitoringLocation), Mar)
results22<-tidy(Anova(model22))
summary(model22)

model23<-lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Apr)
results23<-tidy(Anova(model23))
summary(model23)

model24<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), May)
results24<-tidy(Anova(model24))
summary(model24)

model25<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Jun)
results25<-tidy(Anova(model25))
summary(model25)

model26<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Jul)
results26<-tidy(Anova(model26))
summary(model26)

model27<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Aug)
results27<-tidy(Anova(model27))
summary(model27)

model28<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Sept)
results28<-tidy(Anova(model28))
summary(model28)

model29<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Oct)
results29<-tidy(Anova(model29))
summary(model29)

model30<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Nov)
results30<-tidy(Anova(model30))
summary(model30)

model31<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region+(1|Site) + (1|MonitoringLocation), Dec)
results31<-tidy(Anova(model31))
summary(model31)

## ADJUSTING P VALUE INTENSITY & MONTH ##

## Combining Anova () chi sq stats into a table 
Monthly_Intensity_Results <-rbind(results20, results21, results22, results23, results24, results25, results26, results27, results28, results29, results30, results31)
View(Monthly_Intensity_Results)

### ADJUSTING P VALUE PREVALENCE & MONTH ###

Monthly_Intensity_Results$fdr.p.value <- p.adjust(Monthly_Intensity_Results$p.value, method = "fdr")
Monthly_Intensity_Results
Monthly_Intensity_Results$Month <- c("Jan", "Jan", "Jan", "Feb", "Feb", "Feb", "Mar", "Mar", "Mar", "Apr", "Apr","Apr", "May", "May", "May", "Jun", "Jun", "Jun", "Jul", "Jul","Jul", 
                                      "Aug", "Aug", "Aug", "Sept", "Sept", "Sept", "Oct", "Oct", "Oct", "Nov", "Nov", "Nov", "Dec", "Dec", "Dec")
write.table(Monthly_Intensity_Results, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Monthly_Intensity_Results.csv", sep=",", row.names=FALSE)


