#### Conducting the data by weeks,data,month
system.time(data <- readRDS("./rdsData/covidData.rds"))
library(lubridate)
library(tidyverse)
library(dplyr)
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
############### Creating the weeks Average#############
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

BitCovJoin <- TopConCov %>%
  dplyr::group_by(Country_Region, year,week)%>%
  dplyr::summarize(DeathRate = mean(Deaths),
                   ConfirmedRate = mean(Confirmed),
                   CloseRate = mean(Close))












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