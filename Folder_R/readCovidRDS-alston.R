#
# readCovidRDS.R
# 
# read the COVID data after the files have been combined into
# a single data frame and saved to disk

system.time(data <- readRDS("./rdsData/covidData.rds"))
library(lubridate)
library(tidyverse)

#determining which countries to choose
countries <- as.data.frame(table(data$Country_Region))

#Joining the datasets together (Covid-19 & Bitcoin)- grouping by Country_Region
subsetCovidData <- data %>%
     filter(date > mdy("01/01/2020") & date < mdy("09/01/2022")) %>%
     rename(Date = date) %>%
     # code to summarise() up to one row per day
     group_by(Country_Region,Date) %>% 
     summarise(Confirmed = sum(Confirmed),
               Active = sum(Active),
               Deaths = sum(Deaths),
               Recovered = sum(Recovered)) %>% 
     full_join(.,btcData)

subsetCovidData <- data %>%
  filter(date > mdy("01/01/2020") & date < mdy("08/26/2022")) %>%
  rename(Date = date) %>%
  mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
         Incident_Rate = ifelse(is.na(Incident_Rate),0,Incident_Rate),
         Deaths = ifelse(is.na(Deaths),0,Deaths),
         Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region),
         Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
  # code to summarise() up to one row per country per day
  group_by(Country_Region,Date) %>%
  summarise(Fatality_Rate = sum(Deaths)/ sum(Confirmed)*100,
            Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Incident_Rate = sum(Incident_Rate)) %>% 
  full_join(.,btcData) %>%filter(!is.na(Confirmed)) %>%


#Right merge Write the bitcoin plus the data covid disc
saveRDS(subsetCovidData,"./rdsData/covidBitcoinData.rds")
adataframe <- readRDS("./rdsData/covidBitcoinData.rds")
#checking relation
identical(subsetCovidData,adataframe)
# subset to China data


mut <- mutate(subsetCovidData,Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region))
EconData <- subsetCovidData %>%
  filter(Country_Region %in% c("US","China","Ukraine","Russia","Japan",
                               "Germany","United Kingdom","France"))
View(EconData)

max(subsetCovidData$Date)
min(subsetCovidData$Date)
# AGG--- computes summary statistics for each subsets 
#and returns the result in a group by form
aggregate(Close~Country_Region,EconData,mean)
#Summary Dataset
summary(EconData)
##Missing values
sum(is.na(EconData))
# Data objects
str(EconData)
#Histogram
hist(EconData)
#Visualization
ConfCovid=EconData$Confirmed
Country1= EconData$Country_Region
#Histogram- Must be numeric
hist(ConfCovid)
#Categorical Variables
table(Country1)
barplot(Country1(ConfCovid))
barplot(table(ConfCovid))
barplot(table(Country1))
#Histogram- Must be numeric
hist(ConfCovid)
by(EconData$Confirmed,EconData$Country_Region, summary)
##Boxplot-- Cummulative dataset
boxplot(EconData$Confirmed~EconData$Country_Region)
boxplot(EconData$Confirmed~EconData$Country_Region)+xlab("Countries")+
  ylab("Covid Confirmed")

boxplot(EconData$Confirmed~EconData$Country_Region, notch=T, xlab = "Countries",
        ylab = "Confirmed Covid")
boxplot(EconData$Confirmed~EconData$Country_Region, notch=T,
        col=c("gold","gold","gold","gold","grey","grey","grey","grey"),
        xlab = "Countries",ylab = "Confirmed Covid",
        main= "COnfirmation for Covid-19 Cases")
#Visualization
SubCountry= subsetCovidData$Country_Region
#Categorical Variables- Table output of the countries= 8.. # of observations
#Bar-chart
table(SubCountry)
barplot(table(SubCountry))
##Boxplot Confirmed
boxplot(EconData$Confirmed~EconData$Country_Region, notch=T,
        col=c("gold","gold","gold","gold","grey","grey","grey","grey"),
        xlab = "Countries",ylab = "Confirmed Covid",
        main= "Cumaltive COnfirmation for Covid-19 Cases")

##Cant explain whats going on
plot(x=EconData$Date, y=EconData$Confirmed)

#Boxplot Active
boxplot(EconData$Active~EconData$Country_Region,notch=T,
        col=c("grey","grey","grey","grey","grey","grey","grey","grey"),
        xlab = "Countries",ylab = "Active Covid",
        main= "Active for Covid-19 Cases")

#Histogram
ggplot(aes(x=Country_Region, y=Active),data=plot_data)+
  geom_bar(postion='dodge',stat='identify')+
  scale_y_contiuous(expand=c(0,0))+
  theme_classic()


#  Two-Way ANOVA
# Null Hypothesis... ALPHA=0.5

TwoWayA <- aov(Country_Region~Active + Confirmed + Deaths + Recovered,
               data=EconData)
summary(TwoWayA)

#chinaData <- subsetCovidData %>%
#     filter(Country_Region %in% c("Mainland China","China"))

test <-lm(Close~Active + Confirmed + Deaths,data = EconData)
summary(test)

towAnova<- aov(Close~ Deaths * factor(Country_Region), data=EconData)
summary(towAnova)

##Add another factor by YEAR
ConAnova<- aov(Close~ Confirmed * factor(Country_Region), data=EconData)
summary(ConAnova)

install.packages("corrr")
library(corrr)
library(palmerp)

##has a simular relationship
cor(EconData$Confirmed,EconData$Active,use="complete.obs")
##has a smaller relationship
cor(EconData$Recovered,EconData$Deaths,use="complete.obs")

aov(Confirmed~Deaths,data=EconData)

cor.test(Confirmed, Active, method=c(“pearson”), data=EconData)
