
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
Perkinsus <- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Perkinsus_data_all.csv",)
#View(Perkinsus)

Perk<- na.omit(Perkinsus)
#View(Perk)

 ## Removing sites > 0.5 in distance matrix calculations ####
#Perk <- filter(Perk, Site != 'RAGGED POINT (LC)', Site != 'PARSONS ISLAND', Site != 'PAGAN (S)' , Site != 'OYSTER SHELL PT. (S)')

#Removing Atlantic side regions
Perk<- filter(Perk, Region != 'YEOCOMICO RIVER', Region != 'EASTERN SHORE')

##Intensity individual data from MDDNR- loading and cleaning 
IntensityMD<- read.csv("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/MDDNR_1990-2020.csv",)

as.data.frame(IntensityMD)

#Rebinning intensity scores to follow VIMS

IntensityMD<- IntensityMD %>%
  mutate(Intensity_score = dplyr::recode(Intensity, 
                                 "0" = 0, "1" = 0.5, "2" = 3, "3"= 3, "4"= 3, "5"= 5,"6"= 5, "7"=5))
head(IntensityMD)
IntensityMD <-na.omit(IntensityMD)
#View(IntensityMD)
#Calculating mean intensity for each site and year
IntensityMD$prevalent = ifelse(IntensityMD$Intensity_score == 0, '0',
                            ifelse(IntensityMD$Intensity_score > 0, '1', NA))

present <- nrow(IntensityMD %>% filter(prevalent == '1'))
absent <- nrow(IntensityMD %>% filter(prevalent == '0'))

prevalence_all <- present/(present+absent)
prevalence_all

#View(IntensityMD)

str(IntensityMD$prevalent)
IntensityMD$prevalent <- as.numeric(IntensityMD$prevalent)

Mean_IntensityMD <- IntensityMD %>%
  dplyr::group_by(Region, Site, Year, SampleDate) %>%
  dplyr::summarize(Intensity_final = sum(Intensity_score = as.numeric(as.character(Intensity_score)), 
                            na.rm = TRUE)/sum(prevalent, na.rm=TRUE))

#View(Mean_IntensityMD) # Need to fix Na values 

Mean_IntensityMD <-na.omit(Mean_IntensityMD)

#Update Maryland sites and remerge maryland and VA data
MD <- Perk %>%
  filter(State == "MD")
VA <- Perk %>%
  filter(State =="VA")
View(VA)
MD_converted <- merge(Mean_IntensityMD, MD)
#View(MD_converted)

MD_converted <- MD_converted %>%
  subset(select = -c(Mean.Intensity, SampleDate)) %>%
  dplyr::rename(Mean.Intensity = Intensity_final)

#Site count
Site_count <- MD_converted %>% 
  distinct(Site) %>% 
  nrow()
Site_count #38

Site_count <- VA %>% 
  distinct(Site) %>% 
  nrow()
Site_count  #32
  
#Recombine VA & MD
  
Perk2 <- rbind(VA, MD_converted)  
View(Perk2)

Site_count <- Perk2%>% 
  distinct(Site) %>% 
  nrow()
Site_count #67 


write.table(Perk2, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Perkinsus_data_converted.csv", sep=",", row.names=FALSE)




######################### Environmental Data ###############################

EnvALL<- read_excel("~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/environmental_data_all.xlsx")
#View(EnvALL)
#write.table(Env, file="Env.csv", sep=",", row.names=FALSE)

EnvALL<- filter(EnvALL, SampleDate > '1989-12-31')
#View(EnvALL)

#### creating dates for environmental data ###

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
head(EnvALL)
#View(EnvALL)

EnvALL$Day <-day(EnvALL$SampleDate)
#View(EnvALL)


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



SALINITY <- EnvALL[EnvALL$Parameter == "SALINITY",]

SALINITY <- SALINITY %>%
  dplyr::rename(SALINITY = MeasureValue)
#View(SALINITY)

WTEMP <-EnvALL[EnvALL$Parameter == "WTEMP",]

WTEMP <- WTEMP %>%
  dplyr::rename(WTEMP = MeasureValue)

### REMERGING ENVIRONMENTAL DATA ####

MasterENV<- merge(SALINITY, WTEMP, by =c("Year","Month", "MonitoringLocation", "SampleDate", "Latitude", "Longitude", "Depth", "Layer"), all = TRUE)
#View(MasterENV)

MasterENV<- subset(MasterENV, select = -c(Parameter.x,Parameter.y))

MasterENV<-na.omit(MasterENV)
#View(MasterENV)

### Filtering environmental data so there is only one sample date per month each year ###

class(MasterENV$SampleDate)
MasterENV$SampleDate <- as.Date(MasterENV$SampleDate, format = "%Y-%m-%d")

MasterENV$Month_Year <- format(MasterENV$SampleDate, "%Y-%m")
MasterENV$Month_Day <- format(MasterENV$SampleDate, "%m-%d")


MasterENV_filtered<- MasterENV %>%
  group_by(MonitoringLocation,Year, Month) %>%
  dplyr::slice(1)%>%
  ungroup()
#View(MasterENV_filtered)

##Alternative way ###
#MasterENV <- MasterENV %>%
#  mutate(month_year_monitoringlocation = paste0(format(SampleDate, "%Y-%m"), "-", MonitoringLocation))
#View(MasterENV)

#MasterENV_filtered <- MasterENV %>%
 # group_by(month_year_monitoringlocation) %>%
 # filter(n()==1) %>%
  #ungroup()


envplot<- ggplot(MasterENV_filtered, aes(Year, SALINITY, color = MonitoringLocation))+geom_smooth(se= FALSE)
envplot

# removing sites with missing data from 1990-2020
desired_start_year <- 1990 
site_start_years <- MasterENV_filtered %>%
  group_by(MonitoringLocation) %>%
  summarize(start_year = min(year(SampleDate)))

MasterENV_filtered<- MasterENV_filtered %>%
  inner_join(site_start_years, by= "MonitoringLocation") %>%
  filter(start_year == desired_start_year) %>%
  dplyr::select(-start_year)
#View(MasterENV_filtered)

#write.table(Maryland, file = "~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Filtered_Env_Master.csv", sep = ",", row.names=FALSE)


### Yearly means for environmental parameters by site and time ###


#Smeans <-MasterENV_filtered %>%
#  group_by(Year, Month, SALINITY, MonitoringLocation) %>%
#  summarize(SALINITY = mean(SALINITY))
#View(Smeans)

#### TEMP ###
MonthlyMeans<-MasterENV_filtered %>%
  group_by(Month, Year, WTEMP, MonitoringLocation, SampleDate, Latitude, Longitude) %>%
  dplyr::summarize(WTEMP = mean(WTEMP))
#View(MonthlyMeans)

MonthlyMeans$Month <- month.abb[MonthlyMeans$Month]

head(MonthlyMeans)
#View(MonthlyMeans)

### Salinity ###

MonthlyMeans1<-MasterENV_filtered %>%
  group_by(Month, Year, SALINITY, MonitoringLocation, SampleDate) %>%
  dplyr::summarize(SALINITY = mean(SALINITY))
#View(MonthlyMeans1)

MonthlyMeans1$Month <- month.abb[MonthlyMeans1$Month]

head(MonthlyMeans1)
#View(MonthlyMeans1)

################################## MERGING ENV & PERKINSUS DATA ######################################################
### Merging data sheets using Monthly/yearly means  #### 
Master1<- merge(Perk2, MonthlyMeans, by =c("Year", "MonitoringLocation"))
#View(Master1)

Master1<- Master1 %>% 
  dplyr::rename("Sample.month"= "Month.x")
#View(Master1)

Master1<- Master1 %>% 
  dplyr::rename( "Month"= "Month.y")
#View(Master1)


Merged.data <- merge(MonthlyMeans1, Master1, by =c("Year","Month", "MonitoringLocation", "SampleDate"))
#View(Merged.data)

Merged.data$oysteryear=ifelse(Merged.data$Month== "Nov"| Merged.data$Month=="Dec", Merged.data$Year+1, Merged.data$Year)

head(Merged.data)

#Merged.data <- Merged.data %>% 
 # rename( "SampleDate"= "SampleDate.x")

View(Merged.data)

Merged.data<- Merged.data %>% 
  dplyr::rename( "Lat_Env"= "Latitude")

Merged.data<- Merged.data %>% 
  dplyr::rename( "Long_Env"= "Longitude")
#View(Merged.data)

write.table(Merged.data, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/MergedData.csv", sep=",", row.names=FALSE)

############# Cleaning the data of outliers ################
View(Merged.data)

## Subset MD for Sarah ##

Maryland <- Merged.data %>%
  filter(State == "MD")
View(Maryland)
write.table(Maryland, file = "~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Maryland_data_all.csv", sep = ",", row.names=FALSE)

Site_count <- Merged.data %>% 
  group_by(State, Site) %>%
  distinct(Site) %>% 
  nrow()

Site_count #72


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

Perk2$Mean.Intensity <- as.numeric(Perk2$Mean.Intensity)
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

Merged.data

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

model20<- lmer(Mean.Intensity~ WTEMP + SALINITY +Region + (1|Site) + (1|MonitoringLocation), data = Jan)
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


################################## Regional Models ########################################################

Merged.data$Region

#Subetting data by Region to create individual spreadsheets
Nanticoke <- Merged.data[Merged.data$Region == "NANTICOKE RIVER",]
UpperBay<- Merged.data[Merged.data$Region == "UPPER BAY",]
Chester<- Merged.data[Merged.data$Region == "CHESTER RIVER",]
EasternBay<- Merged.data[Merged.data$Region == "EASTERN BAY",]
Wye<- Merged.data[Merged.data$Region == "WYE RIVER",]
Miles<- Merged.data[Merged.data$Region == "MILES RIVER",]
BroadCreek<- Merged.data[Merged.data$Region == "BROAD CREEK",]
HarrisCreek<- Merged.data[Merged.data$Region == "HARRIS CREEK",]
Choptank<- Merged.data[Merged.data$Region == "CHOPTANK RIVER",]
LittleChoptank<- Merged.data[Merged.data$Region == "LITTLE CHOPTANK RIVER",]
Patuxent<- Merged.data[Merged.data$Region == "PATUXENT RIVER",]
MiddleBay<- Merged.data[Merged.data$Region == "MIDDLE BAY",]
Manokin <- Merged.data[Merged.data$Region == "MANOKIN RIVER",]
Potomac<- Merged.data[Merged.data$Region == "POTOMAC RIVER",]
LowerBay<- Merged.data[Merged.data$Region == "LOWER BAY",]
FishingBay<- Merged.data[Merged.data$Region == "FISHING BAY",]
Honga<- Merged.data[Merged.data$Region == "HONGA RIVER",]
TangierS<- Merged.data[Merged.data$Region == "TANGIER SOUND",]
HollandS<- Merged.data[Merged.data$Region == "HOLLAND STRAITS",]
PocomokeS<- Merged.data[Merged.data$Region == "POCOMOKE SOUND",]
Rappahannock<- Merged.data[Merged.data$Region == "RAPPAHANNOCK RIVER",]
GWicomico<- Merged.data[Merged.data$Region == "GREAT WICOMICO RIVER",]
Corrotoman<- Merged.data[Merged.data$Region == "CORROTOMAN RIVER",]
Piankatank<- Merged.data[Merged.data$Region == "PIANKATANK RIVER",]
York<- Merged.data[Merged.data$Region == "YORK RIVER",]
Mobjack<- Merged.data[Merged.data$Region == "MOBJACK BAY" | Merged.data$Region == "MOBJACK RIVER",]
James<- Merged.data[Merged.data$Region == "JAMES RIVER",]
Stmary<- Merged.data[Merged.data$Region == "ST. MARY'S RIVER",]

#Removed EASTERN SHORE & YEOCOMICO RIVER

Region_count <- Merged.data %>% 
  #group_by(State, Region) %>%
  distinct(Region) %>% 
  nrow()

Region_count #29

list(Merged.data$Region)

### Prevalence ##
model32<- glmmTMB(Prev_ep~ WTEMP * SALINITY+ (1|Site)+ (1|MonitoringLocation), Nanticoke, family = beta_family())
results32<-tidy(Anova(model32))
summary(model32)

model33<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), UpperBay, family = beta_family())
results33<-tidy(Anova(model33))
summary(model33)

model34<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Chester, family = beta_family())
results34<-tidy(Anova(model34))
summary(model34)

model35<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), EasternBay, family = beta_family())
results35<-tidy(Anova(model35))
summary(model35)

model36<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Wye, family = beta_family())
results36<-tidy(Anova(model36))
summary(model36)

model37<- glmmTMB(Prev_ep~ WTEMP * SALINITY+ (1|Site)+ (1|MonitoringLocation), Miles, family = beta_family())
results37<-tidy(Anova(model37))
summary(model37)

model38<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), BroadCreek, family = beta_family())
results38<-tidy(Anova(model38))
summary(model38)

model39<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), HarrisCreek, family = beta_family())
results39<-tidy(Anova(model39))
summary(model39)

model40<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Choptank, family = beta_family())
results40<-tidy(Anova(model40))
summary(model39)

model41<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), LittleChoptank, family = beta_family())
results41<-tidy(Anova(model41))
summary(model41)

model42<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Patuxent, family = beta_family())
results42<-tidy(Anova(model42))
summary(model42)

model43<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), MiddleBay, family = beta_family())
results43<-tidy(Anova(model43))
summary(model43)

model44<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Manokin, family = beta_family())
results44<-tidy(Anova(model44))
summary(model44)

model45<- glmmTMB(Prev_ep~ WTEMP * SALINITY+ (1|Site)+ (1|MonitoringLocation), Potomac, family = beta_family())
results45<-tidy(Anova(model45))
summary(model45)


model46<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), LowerBay, family = beta_family())
results46<-tidy(Anova(model46))
summary(model46)

model47<- glmmTMB(Prev_ep~ WTEMP*SALINITY + (1|Site)+ (1|MonitoringLocation), FishingBay, family = beta_family())
results47<-tidy(Anova(model47))
summary(model47)


model48<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Honga, family = beta_family())
results48<-tidy(Anova(model48))
summary(model48)

model49<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), TangierS, family = beta_family())
results49<-tidy(Anova(model49))
summary(model49)

model50<- glmmTMB(Prev_ep~ WTEMP * SALINITY+ (1|Site)+ (1|MonitoringLocation), HollandS, family = beta_family())
results50<-tidy(Anova(model50))
summary(model50)

model51<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), PocomokeS, family = beta_family())
results51<-tidy(Anova(model51))
summary(model51)

model52<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Rappahannock, family = beta_family())
results52<-tidy(Anova(model52))
summary(model52)

model53<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), GWicomico, family = beta_family())
results53<-tidy(Anova(model53))
summary(model53)

model54<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Corrotoman, family = beta_family())
results54<-tidy(Anova(model54))
summary(model54)

model55<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Piankatank, family = beta_family())
results55<-tidy(Anova(model55))
summary(model55)

model56<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), York, family = beta_family())
results56<-tidy(Anova(model56))
summary(model56)

model57<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation),Mobjack, family = beta_family())
results57<-tidy(Anova(model57))
summary(model57)

model58<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), James, family = beta_family())
results58<-tidy(Anova(model58))
summary(model58)

model59<- glmmTMB(Prev_ep~ WTEMP * SALINITY + (1|Site)+ (1|MonitoringLocation), Stmary, family = beta_family())
results59<-tidy(Anova(model59))
summary(model59)
## Combining Anova () chi sq stats into a table 
Regional_Prevalence_Results <-rbind(results32, results33, results34, results35, results36, results37, results38, results39, results40, results41, results42, results43,
                                    results44, results45, results46, results47, results48, results49, results50, results51, results52, results53, results54, results55, results56,
                                    results57, results58, results59)
View(Regional_Prevalence_Results)

### ADJUSTING P VALUE PREVALENCE & MONTH ###

Regional_Prevalence_Results$fdr.p.value <- p.adjust(Regional_Prevalence_Results$p.value, method = "fdr")
Regional_Prevalence_Results
#Regional_Prevalence_Results$Region <- c("NANTICOKE RIVER","NANTICOKE RIVER", "UPPER BAY", "UPPER BAY","CHESTER RIVER", "CHESTER RIVER","EASTERN BAY", "EASTERN BAY",
#                                        "WYE RIVER", "WYE RIVER","MILES RIVER","MILES RIVER", "BROAD CREEK","BROAD CREEK",
#                                        "HARRIS CREEK", "HARRIS CREEK","CHOPTANK RIVER","CHOPTANK RIVER", "LITTLE CHOPTANK RIVER","LITTLE CHOPTANK RIVER",
#                                        "PATUXENT RIVER","PATUXENT RIVER", "MIDDLE BAY",  "MIDDLE BAY","MANOKIN RIVER","MANOKIN RIVER",
#                                        "POTOMAC RIVER","POTOMAC RIVER", "LOWER BAY","LOWER BAY","FISHING BAY","FISHING BAY", "HONGA RIVER","HONGA RIVER",
#                                       "TANGIER SOUND", "TANGIER SOUND","HOLLAND STRAITS","HOLLAND STRAITS","POCOMOKE SOUND","POCOMOKE SOUND",
#                                        "RAPPAHANNOCK RIVER","RAPPAHANNOCK RIVER","GREAT WICOMICO RIVER","GREAT WICOMICO RIVER",
#                                        "CORROTOMAN RIVER","CORROTOMAN RIVER","PIANKATANK RIVER","PIANKATANK RIVER","YORK RIVER","YORK RIVER",
#                                         "MOBJACK BAY","MOBJACK BAY","JAMES RIVER","JAMES RIVER","ST MARY'S RIVER", "ST MARY'S RIVER")

Regional_Prevalence_Results$Region <- c("NANTICOKE RIVER","NANTICOKE RIVER", "NANTICOKE RIVER", "UPPER BAY", "UPPER BAY","UPPER BAY","CHESTER RIVER", "CHESTER RIVER","CHESTER RIVER","EASTERN BAY", "EASTERN BAY","EASTERN BAY",
                                        "WYE RIVER", "WYE RIVER","WYE RIVER","MILES RIVER","MILES RIVER","MILES RIVER", "BROAD CREEK","BROAD CREEK","BROAD CREEK",
                                        "HARRIS CREEK", "HARRIS CREEK","HARRIS CREEK","CHOPTANK RIVER","CHOPTANK RIVER","CHOPTANK RIVER", "LITTLE CHOPTANK RIVER","LITTLE CHOPTANK RIVER","LITTLE CHOPTANK RIVER",
                                        "PATUXENT RIVER","PATUXENT RIVER","PATUXENT RIVER", "MIDDLE BAY",  "MIDDLE BAY","MIDDLE BAY","MANOKIN RIVER","MANOKIN RIVER","MANOKIN RIVER",
                                        "POTOMAC RIVER","POTOMAC RIVER","POTOMAC RIVER", "LOWER BAY","LOWER BAY","LOWER BAY","FISHING BAY","FISHING BAY","FISHING BAY", "HONGA RIVER","HONGA RIVER","HONGA RIVER",
                                        "TANGIER SOUND", "TANGIER SOUND","TANGIER SOUND","HOLLAND STRAITS","HOLLAND STRAITS","HOLLAND STRAITS","POCOMOKE SOUND","POCOMOKE SOUND","POCOMOKE SOUND",
                                        "RAPPAHANNOCK RIVER","RAPPAHANNOCK RIVER","RAPPAHANNOCK RIVER","GREAT WICOMICO RIVER","GREAT WICOMICO RIVER","GREAT WICOMICO RIVER",
                                        "CORROTOMAN RIVER","CORROTOMAN RIVER","CORROTOMAN RIVER","PIANKATANK RIVER","PIANKATANK RIVER","PIANKATANK RIVER","YORK RIVER","YORK RIVER","YORK RIVER",
                                        "MOBJACK BAY","MOBJACK BAY","MOBJACK BAY","JAMES RIVER","JAMES RIVER","JAMES RIVER","ST MARY'S RIVER", "ST MARY'S RIVER","ST MARY'S RIVER")
write.table(Regional_Prevalence_Results, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Regional_Prevalence_Results_Interactions.csv", sep=",", row.names=FALSE)

##### YEAR

model60<- glmmTMB(Prev_ep~ Year , Nanticoke, family = beta_family())
results60<-tidy(Anova(model60))
summary(model60)

model61<- glmmTMB(Prev_ep~ Year , UpperBay, family = beta_family())
results61<-tidy(Anova(model61))
summary(model61)


model62<- glmmTMB(Prev_ep~ Year , Chester, family = beta_family())
results62<-tidy(Anova(model62))
summary(model62)

model63<- glmmTMB(Prev_ep~ Year , EasternBay, family = beta_family())
results63<-tidy(Anova(model63))
summary(model63)

model64<- glmmTMB(Prev_ep~ Year , Wye, family = beta_family())
results64<-tidy(Anova(model64))
summary(model64)

model65<- glmmTMB(Prev_ep~  Year , Miles, family = beta_family())
results65<-tidy(Anova(model65))
summary(model65)

model66<- glmmTMB(Prev_ep~ Year , BroadCreek, family = beta_family())
results66<-tidy(Anova(model66))
summary(model66)

model67<- glmmTMB(Prev_ep~ Year , HarrisCreek, family = beta_family())
results67<-tidy(Anova(model67))
summary(model67)

model68<- glmmTMB(Prev_ep~ Year , Choptank, family = beta_family())
results68<-tidy(Anova(model68))
summary(model68)

model69<- glmmTMB(Prev_ep~ Year , LittleChoptank, family = beta_family())
results69<-tidy(Anova(model69))
summary(model69)

model70<- glmmTMB(Prev_ep~ Year, Patuxent, family = beta_family())
results70<-tidy(Anova(model70))
summary(model70)

model71<- glmmTMB(Prev_ep~ Year , MiddleBay, family = beta_family())
results71<-tidy(Anova(model71))
summary(model71)

model72<- glmmTMB(Prev_ep~ Year , Manokin, family = beta_family())
results72<-tidy(Anova(model72))
summary(model72)

model73<- glmmTMB(Prev_ep~ Year, Potomac, family = beta_family())
results73<-tidy(Anova(model73))
summary(model73)


model74<- glmmTMB(Prev_ep~ Year, LowerBay, family = beta_family())
results74<-tidy(Anova(model74))
summary(model74)

model75<- glmmTMB(Prev_ep~Year , FishingBay, family = beta_family())
results75<-tidy(Anova(model75))
summary(model75)


model76<- glmmTMB(Prev_ep~ Year , Honga, family = beta_family())
results76<-tidy(Anova(model76))
summary(model76)

model77<- glmmTMB(Prev_ep~ Year , TangierS, family = beta_family())
results77<-tidy(Anova(model77))
summary(model77)

model78<- glmmTMB(Prev_ep~ Year, HollandS, family = beta_family())
results78<-tidy(Anova(model78))
summary(model78)

model79<- glmmTMB(Prev_ep~ Year , PocomokeS, family = beta_family())
results79<-tidy(Anova(model79))
summary(model79)

model80<- glmmTMB(Prev_ep~ Year , Rappahannock, family = beta_family())
results80<-tidy(Anova(model80))
summary(model80)

model81<- glmmTMB(Prev_ep~ Year, GWicomico, family = beta_family())
results81<-tidy(Anova(model81))
summary(model81)

model82<- glmmTMB(Prev_ep~ Year , Corrotoman, family = beta_family())
results82<-tidy(Anova(model82))
summary(model82)

model83<- glmmTMB(Prev_ep~ Year , Piankatank, family = beta_family())
results83<-tidy(Anova(model83))
summary(model83)

model84<- glmmTMB(Prev_ep~ Year, York, family = beta_family())
results84<-tidy(Anova(model84))
summary(model84)

model85<- glmmTMB(Prev_ep~ Year ,Mobjack, family = beta_family())
results85<-tidy(Anova(model85))
summary(model85)

model86<- glmmTMB(Prev_ep~ Year , James, family = beta_family())
results86<-tidy(Anova(model86))
summary(model86)

model87<- glmmTMB(Prev_ep~ Year , Stmary, family = beta_family())
results87<-tidy(Anova(model87))
summary(model87)
## Combining Anova () chi sq stats into a table 
Regional_PrevalenceYear_Results <-rbind(results60, results61, results62, results63, results64, results65, results66, results67, results68, results69, results70, results71,
                                    results72, results73, results74, results75, results76, results77, results78, results79, results80, results81, results82, results83, results84,
                                    results85, results86, results87)
View(Regional_Prevalence_Results)

### ADJUSTING P VALUE PREVALENCE & MONTH ###

Regional_PrevalenceYear_Results$fdr.p.value <- p.adjust(Regional_PrevalenceYear_Results$p.value, method = "fdr")
Regional_PrevalenceYear_Results
Regional_PrevalenceYear_Results$Region <- c("NANTICOKE RIVER", "UPPER BAY", "CHESTER RIVER", "EASTERN BAY",
                                       "WYE RIVER", "MILES RIVER", "BROAD CREEK",
                                        "HARRIS CREEK", "CHOPTANK RIVER", "LITTLE CHOPTANK RIVER",
                                       "PATUXENT RIVER", "MIDDLE BAY",  "MANOKIN RIVER",
                                       "POTOMAC RIVER","LOWER BAY","FISHING BAY", "HONGA RIVER",
                                      "TANGIER SOUND", "HOLLAND STRAITS","POCOMOKE SOUND",
                                       "RAPPAHANNOCK RIVER","GREAT WICOMICO RIVER",
                                       "CORROTOMAN RIVER","PIANKATANK RIVER","YORK RIVER",
                                        "MOBJACK BAY","JAMES RIVER","ST MARY'S RIVER")
write.table(Regional_PrevalenceYear_Results, file="~/Documents/UMBC/GitHub/Meta-analysis-Perkinsus-Marinus/Data Files/Regional_PrevalenceYear_Results.csv", sep=",", row.names=FALSE)

### Pearson's correlation
### Pearson's Correlation- Temperature and Salinity ### https://www.r-bloggers.com/2021/10/pearson-correlation-in-r/
# r value of 0-0.3 = not correlated

#Running pearson's correlation for Manokin River, James River, Honga River, and Broad Creek as they were significant for T:S and Year for Regional models

#Temperature vs Salinity
ManokinRiverTvS<- cor(Manokin$WTEMP, Manokin$SALINITY, method= 'pearson')
ManokinRiverTvS
#-0.1131532
HongaRiverTvS<- cor(Honga$WTEMP, Honga$SALINITY, method= 'pearson')
HongaRiverTvS
#-0.1650143
JamesRiverTvS<- cor(James$WTEMP, James$SALINITY, method= 'pearson')
JamesRiverTvS
#0.1437193 NOT CORRELATED
BroadCreekTvS<- cor(BroadCreek$WTEMP, BroadCreek$SALINITY, method= 'pearson')
BroadCreekTvS
#-0.2011958

#Temperature vs Year
ManokinRiverTvY<- cor(Manokin$WTEMP, Manokin$Year, method= 'pearson')
ManokinRiverTvY
#0.02633882 - NOT correlated
HongaRiverTvY<- cor(Honga$WTEMP, Honga$Year, method= 'pearson')
HongaRiverTvY
#0.02131326- NOT correlated
JamesRiverTvY<- cor(James$WTEMP, James$Year, method= 'pearson')
JamesRiverTvY
#0.05735578
BroadCreekTvY<- cor(BroadCreek$WTEMP, BroadCreek$Year, method= 'pearson')
BroadCreekTvY
#0.01601823- NOT correlated

#Salinity vs Year
ManokinRiverYvS<- cor(Manokin$Year, Manokin$SALINITY, method= 'pearson')
ManokinRiverYvS
#-0.08819826 
HongaRiverYvS<- cor(Honga$Year, Honga$SALINITY, method= 'pearson')
HongaRiverYvS
#-0.003681306
JamesRiverYvS<- cor(James$Year, James$SALINITY, method= 'pearson')
JamesRiverYvS
#0.05585008 - NOT CORRELATED
BroadCreekYvS<- cor(BroadCreek$Year, BroadCreek$SALINITY, method= 'pearson')
BroadCreekYvS
#-0.06011818 
