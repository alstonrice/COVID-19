#### Conducting the data by weeks,data,month
system.time(data <- readRDS("./rdsData/covidData.rds"))
library(lubridate)
library(tidyverse)
library(dplyr)
library(MASS)
library(haven)
#determining which countries to choose
countries <- as.data.frame(table(data$Country_Region))
max(countries$Freq,countries$Var1)

TopCountry <- countries %>%
  arrange(desc(Freq)) %>%
  group_by(Var1)
TopCountry   
Coun <- data %>%
  filter(date== mdy("08/30/2022"))%>%
  mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
         Deaths = ifelse(is.na(Deaths),0,Deaths))

Top <- Coun %>%
  arrange(desc(Confirmed)) %>%
  group_by(Country_Region)
Top
############### Creating the weeks Average#####################################
TestRun <- data %>%
  filter(date > mdy("01/01/2020") & date < mdy("09/01/2022")) %>%
  rename(Date = date) %>% 
  mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
         Deaths = ifelse(is.na(Deaths),0,Deaths),
         Incident_Rate = ifelse(is.na(Incident_Rate),0,Incident_Rate),
         Country_Region = case_when(Country_Region=="Mainland China" ~ "China",
                                    TRUE ~ Country_Region))%>%
  # code to summarise() up to one row per day
  group_by(Country_Region,Date) %>%
  summarise(Fatality_Rate = sum(Deaths)/ sum(Confirmed)*100,
            Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Incident_Rate = sum(Incident_Rate)) %>% 
  full_join(.,btcData) %>%filter(!is.na(Confirmed))

### stucturing data with certain Countries

mut <- mutate(TestRun,Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region))
CasesData <- mut %>%
  filter(Country_Region %in% c("US","China","Ukraine","Russia","Japan",
                               "India","Gremany","France","South Korea","Canada"))
view(CasesData)


CasesData %>%
  filter(Country_Region %in% c("US","China","Ukraine","Russia","Japan",
                               "India","Germany","France","South Korea","Canada")) %>%
  group_by(Country_Region,Date) %>%  
  summarise(Confirmed = sum(Confirmed),
            Incident_Rate = mean(Incident_Rate),
            Deaths = sum(Deaths),
            #### Calculating the rate to start at Jan 1st- May 29. 2020
            ### for the missing rows not calculating 
            Fatality_Ratio = sum(Deaths) / sum(Confirmed * 100)) %>%
  full_join(.,btcData) %>% filter(!is.na(Confirmed)) %>%
  mutate(time = Date - mdy("01/21/2020"),
         month = month(Date),
         year = year(Date),
         week = week(Date),
         dailyConfirmed = if_else(Date == mdy("01/22/2022"),Confirmed,Confirmed - lag(Confirmed)),
         dailyDeaths = if_else(Date == mdy("01/22/2022"),Deaths,Deaths - lag(Deaths)))-> TopConCov
######Variable analysis for the countries
plot(DeathRate~Country_Region, data=BitCovJoin)

### Group AVG by Country_Region, Year, Week
BitCovJoin <- TopConCov %>%
  dplyr::group_by(Country_Region, year,week)%>%
  dplyr::summarize(DeathRate = mean(Deaths),
                   ConfirmedRate = mean(Confirmed),
                   CloseRate = mean(Close),
                   Incident_Rate = mean(Incident_Rate))

##### Group the AVG by WEEK & YEAR.. Countries are NOT used.. Run to see by 
##### training data 2020-2021
#####test data 2022
WeekYear1 <- TopConCov %>%
  dplyr::group_by(year,week)%>%
  dplyr::summarize(DeathRate = mean(Deaths),
                   ConfirmedRate = mean(Confirmed),
                   CloseRate = mean(Close),
                   Incident_Rate = mean(Incident_Rate)) %>% 
### Ungroup to create W_S as a time series to count the rows  
  ungroup() %>%
  mutate(week_squence = seq(1,n()))

summary(WeekYear1)

library(haven)
plot(DeathRate~Country_Region,data=WeekYear1)

###########MEAN_ AVG  is above#######################################################


test4 <- aov(CloseRate~Country_Region, data = BitCovJoin)
summary(test4)

### countries are based off of the last country= Canadaa
test5 <- lm(CloseRate~ .,data=BitCovJoin)
summary(test5)

### the rates for the certain countries will be the same due to covid
boxplot(CloseRate~Country_Region, data= BitCovJoin)

### inflation starting to occured in 2021.. due to covid (people couldnt work, go out )
### stockmarket goes down due to people worry
boxplot(CloseRate~year, data=BitCovJoin)

boxplot(Incident_Rate~ year,data=BitCovJoin)
##########Checking the boxplot of the Countries and the activities of the Var.
boxplot(DeathRate ~ Country_Region, data=BitCovJoin)

########Cheacking how the Countries are Rated by the variables 
####### To determine the best 3 Countries to pick from
#######Using the Tukey to see which Countries have a low P-Value##################
Death.aov<- aov(DeathRate ~ Country_Region, data=BitCovJoin)
summary(Death.aov)
boxplot(DeathRate ~ Country_Region, data=BitCovJoin)
#Pst Hoc test to find the different countries
TukeyHSD(Death.aov)

Confirm.aov <- aov(ConfirmedRate ~ Country_Region, data=BitCovJoin)
summary(Confirm.aov)
boxplot(ConfirmedRate~ Country_Region,data=BitCovJoin)
TukeyHSD(Confirm.aov)

Incident.aov <- aov(Incident_Rate ~ Country_Region, data=BitCovJoin)
summary(Incident.aov)
boxplot(Incident_Rate~ Country_Region,data=BitCovJoin)
TukeyHSD(Incident.aov)


### What model should not be used#####################################################
Full.Model<- lm(CloseRate~ Country_Region + year + DeathRate + Incident_Rate
                + ConfirmedRate, data=BitCovJoin)
summary(Full.Model)

step.model <- Full.Model %>%
  stepAIC(trace=FALSE)

Step.M <- stepAIC(Full.Model, direction = "both")
###SUmmary shows the stepIAC.. only variables used are (Year,Death,IncidentRate)
summary(step.model)
##### Must be less than 5 for no col linearity 
vif(step.model)
coef(step.model)
#### Testing grouping without country
### Week and year showa no small values
##### Practice by weeks
Full.ModelNoC <- lm(CloseRate~. , data=WeekYear1)
summary(Full.ModelNoC)

step.model1 <- Full.ModelNoC %>%
  stepAIC(trace=FALSE)

Step.M1 <- stepAIC(Full.ModelNoC, direction = "both")
###only 2020 & 2021
traindata1<- WeekYear1[1:103,] ## copy the first 103 rows
str(traindata1)

#last rows to 138... only 2022
testdata1<- WeekYear1[104:138,]
Trainsample <- lm(CloseRate~ year + DeathRate + Incident_Rate
                  + ConfirmedRate, data=traindata1)
summary(Trainsample)
sBitTrain1<-stepAIC(Trainsample,direction="both")
summary(sBitTrain1)

predictBitcoin1<-predict(sBitTrain1,testdata1)
predictBitcoin1

#Add predictBitcoin as a column to testdata
testdata1["Predicted"] <- predictBitcoin1


View(testdata1)
##Plot Actual and Predicted Bitcoin data
plot(testdata1$Predicted,testdata1$CloseRate)
## Split data between train and test/ Validation
###First 776 rows will be used for training a multiple linear regression 
## training the data from the dates
### which date do I want to stop at?
traindata<- BitCovJoin[1:776,] ## copy the first 776 rows
str(traindata)

#last rows to 1109
testdata<- BitCovJoin[777:1109,] #Copy the last rows

# Run regression model with all the independent variables in the model
BitTrain1<- lm(CloseRate~ year + DeathRate + Incident_Rate
              + ConfirmedRate, data=traindata)
summary(BitTrain1)
#Run a stepAIC model
stepBitTrain1<-stepAIC(BitTrain1,direction="both")
## 47% of the variation of the variables
summary(stepBitTrain1)

############################ Validate with the test data
### predicting a model on the Bitcoin data
## ask the model to run by the test dataset
predictBitcoin<-predict(stepBitTrain1,testdata)
predictBitcoin

#Add predictBitcoin as a column to testdata
testdata["Predicted"] <- predictBitcoin

View(testdata)
##Plot Actual and Predicted Bitcoin data
plot(testdata$Predicted,testdata$CloseRate)

install.packages("forecast")
library(fpp2)
library(forecast)
library(tseries)
Time <- ts(WeekYear1$CloseRate,start=c(2020),frequency=54)
autoplot(Time)










##################################################################################
test6 <- lm(CloseRate~ year + DeathRate + Incident_Rate, data = BitCovJoin)
summary(test6)

   

##########################Splitting the data by the AVERAGE separately###
RegionCov <- data %>%
  filter(Country_Region %in% c("US","China","Ukraine","Russia","Japan",
                               "India","United Kingdom","France","Ukraine",
                               "South Korea","Canada","Germany"))

Covidtest <- RegionCov %>%
  filter(date > mdy("01/01/2020") & date < mdy("08/26/2022")) %>%
  mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
         Incident_Rate = ifelse(is.na(Incident_Rate),0,Incident_Rate),
         Deaths = ifelse(is.na(Deaths),0,Deaths),
         Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region),
         Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
  # code to summarise() up to one row per country per day
  summarise(Fatality_Rate = sum(Deaths)/ sum(Confirmed)*100,
            Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Incident_Rate = sum(Incident_Rate)) %>% 
  filter(!is.na(Confirmed))
df_covid <- RegionCov%>%
  dplyr::group_by(Country_Region)

RegionCov %>%
  summarise(Confirmed = sum(Confirmed),
            Incident_Rate = mean(Incident_Rate),
            Deaths = sum(Deaths),
            #### Calculating the rate to start at Jan 1st- May 29. 2020
            ### for the missing rows not calculating 
            Fatality_Ratio = sum(Deaths) / sum(Confirmed * 100)) %>%
  filter(!is.na(Confirmed)) %>%
  mutate(time = date - mdy("01/21/2020"),
         month = month(date),
         year = year(date),
         week = week(date))-> Test_Ex