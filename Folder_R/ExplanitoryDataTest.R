# number Confirmed cases
#### averaging the covid cases by day for eat country to combine
aggregate(Active~Country_Region + Deaths+Recovered,subsetCovidData,sum())

####Create daily variables and time series

#CovidBit <- DataByDate %>%
  mutate(time = Date - myd("01/20/2020"),
         month = month(Date),
         year = year(Date),
         dailyActive = if_else(Date == mdy("01/22/2022"),Active,Active - lag(Active)),
         dailyConfirmed = if_else(Date == mdy("01/22/2022"),Confirmed,Confirmed - lag(Confirmed)),
         dailyDeaths = if_else(Date == mdy("01/22/2022"),Deaths,Deaths - lag(Deaths))) %>%
#  select(Date,Close,dailyActive,dailyConfirmed,dailyDeaths,time,month,year)


###### adding Incident
CovidRate <- data %>%
  filter(date > mdy("01/01/2020") & date < mdy("09/01/2022")) %>%
  rename(Date = date) %>% 
  mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
           Active = ifelse(is.na(Active),0,Active),
           Deaths = ifelse(is.na(Deaths),0,Deaths),
           Recovered = ifelse(is.na(Recovered),0,Recovered),
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
 
save(subsetCovidRate,"./rdsData/ExplanitoryDataTest.rds")

###creating variables for the day, month, and year

Bc <- readRDS("./rdsData/ExplanitoryDataTest.rds")

library(lubridate)
library(tidyverse)

source("./r/readBitcionPrice.R")

CovidRate %>%
  group_by(Date) %>%  
  summarise(Confirmed = sum(Confirmed),
            Incident_Rate = mean(Incident_Rate),
            Deaths = sum(Deaths),
            Fatality_Ratio = sum(Deaths) / sum(Confirmed * 100)) %>%
  full_join(.,btcData) %>% filter(!is.na(Confirmed)) %>%
  mutate(time = Date - mdy("01/21/2020"),
         month = month(Date),
         year = year(Date),
         dailyConfirmed = if_else(Date == mdy("01/22/2022"),Confirmed,Confirmed - lag(Confirmed)),
         dailyDeaths = if_else(Date == mdy("01/22/2022"),Deaths,Deaths - lag(Deaths)))-> TestCovBit
view(TestCovBit)

##modeling

max(TestCovBit$Date)
min(TestCovBit$Date)

#linear relationship
plot(Confirmed~Deaths, data=TestCovBit)

### the correlation is very close to 1
cor(TestCovBit$Confirmed,TestCovBit$Deaths)

### there realtionship is -.75, which is close to -1
cor(TestCovBit$Fatality_Ratio,TestCovBit$Incident_Rate)

## boxplot shows the effect that Covid had on Bitcoin
boxplot(TestCovBit$Close~TestCovBit$year,notch=1,col=c("grey","gold","blue"),
        xlab = "Covid Year", ylab="Bitcoin Close",
        main="Bitcoin Closing for the Years")

##?
boxplot(TestCovBit$Close~TestCovBit$Incident_Rate)

library(corrgram)

test1 <-lm(Close~dailyConfirmed + dailyDeaths +time, data = TestCovBit)
summary(test1)

test2 <-lm(Close~Fatality_Ratio+ Incident_Rate +dailyConfirmed + dailyDeaths +time, data = TestCovBit)
summary(test2)