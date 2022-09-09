# number Confirmed cases
#### averaging the covid cases by day for eat country to combine
aggregate(Active~Country_Region + Deaths+Recovered,subsetCovidData,sum())
DataByDate <- data %>%
  filter(Country_Region %in% c("Mainland China","China","United Kingdom","Ukraine",
                               "US","Russia","Japan","Germany","France"))%>%
  filter(date > mdy("01/01/2020") & date < mdy("09/01/2022")) %>%
  rename(Date = date) %>%
  ##avoid issues
  mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
         Active = ifelse(is.na(Active),0,Active),
         Deaths = ifelse(is.na(Deaths),0,Deaths),
         Recovered = ifelse(is.na(Recovered),0,Recovered))%>%
                              
  ##Summarizing the Countries by one day
  
  group_by(Date) %>% 
  summarise(Confirmed = sum(Confirmed),
            Active = sum(Active),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered)) %>% 
  full_join(.,btcData) %>%filter(!is.na(Confirmed))


####Create daily variables and time series

CovidBit <- DataByDate %>%
  mutate(time = Date - myd("01/20/2020"),
         month = month(Date),
         year = year(Date),
         dailyActive = if_else(Date == mdy("01/22/2022"),Active,Active - lag(Active)),
         dailyConfirmed = if_else(Date == mdy("01/22/2022"),Confirmed,Confirmed - lag(Confirmed)),
         dailyDeaths = if_else(Date == mdy("01/22/2022"),Deaths,Deaths - lag(Deaths))) %>%
  select(Date,Close,dailyActive,dailyConfirmed,dailyDeaths,time,month,year)

library(corrgram)

CovidRate <- data %>%
  filter(date > mdy("01/01/2020") & date < mdy("09/01/2022")) %>%
  rename(Date = date) %>% 
    mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
           Active = ifelse(is.na(Active),0,Active),
           Deaths = ifelse(is.na(Deaths),0,Deaths),
           Recovered = ifelse(is.na(Recovered),0,Recovered),
           Incident_Rate = ifelse(is.na(Incident_Rate),0,Incident_Rate),
           Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region))%>%
  # code to summarise() up to one row per day
  group_by(Country_Region,Date) %>%
  summarise(Fatality_Rate = sum(Deaths)/ sum(Confirmed)*100,
            Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Incident_Rate = sum(Incident_Rate)) %>% 
  full_join(.,btcData)


###creating variables for the day, month, and year

CovidRate %>%
  group_by(Date) %>%
  summarize(Confirmed = sum(Confirmed),
            Indcident_Rate = mean(Indcident_Rate),
            Fatality_Rate = sum(Deaths)/ sum(Confirmed)*100,
            Death = sum(Deaths)) %>%
  full_join(.,btcData) %>%
  mutate(time = Date - myd("01/20/2020"),
         month = month(Date),
         year = year(Date),
         dailyConfirmed = if_else(Date == mdy("01/22/2022"),Confirmed,Confirmed - lag(Confirmed)),
         dailyDeaths = if_else(Date == mdy("01/22/2022"),Deaths,Deaths - lag(Deaths)))

##modeling

Modeltest <-lm(Close~dailyActive + dailyConfirmed + dailyDeaths +time, data = CovidBit)
summary(Modeltest)

