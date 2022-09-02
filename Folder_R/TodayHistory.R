directory <- "./csse_covid_19_data/csse_covid_19_daily_reports/"
# read all files in directory
theFiles <- list.files(path=directory,pattern="*.csv",full.names = TRUE)
filenames <- list.files(path=directory,pattern="*.csv")
# read and clean column information, add missing columns for earliest data
# for details, see ./R/analyzeColumnNamesByDay.R in github repository
library(lubridate)
dataList <- lapply(1:length(theFiles),function(x){
y <-read.csv(theFiles[x],stringsAsFactors=FALSE)
# clean column names and add missing columns
fileDate <- mdy(substr(filenames[x],1,10))
# common processing across all files
colnames(y) <- gsub("\\.","_",colnames(y))
colnames(y) <- sub("ï__","",colnames(y))
# processing specific to first batch of files
if(fileDate < mdy("02/01/2020")) {
y$Province_State <- NA
y$Lat <- NA
y$Long_ <- NA
y$Active <- NA
y$Admin2 <- NA
y$FIPS <- NA
y$Combined_Key <- NA
y$Case_Fatality_Ratio <- NA
y$Incident_Rate <- NA
} else if (fileDate < mdy("03/01/2020")){
# cleaning specific to first wave of files
y$Lat <- NA
y$Long_ <- NA
y$Active <- NA
y$Admin2 <- NA
y$FIPS <- NA
y$Combined_Key <- NA
y$Case_Fatality_Ratio <- NA
y$Incident_Rate <- NA
} else if(fileDate < mdy("03/22/2020")) {
colnames(y) <- sub("Latitude","Lat",colnames(y))
colnames(y) <- sub("Longitude","Long_",colnames(y))
colnames(y) <- sub("Last.Update","Last_Update",colnames(y))
colnames(y) <- sub("Province.State","Province_State",colnames(y))
colnames(y) <- sub("Longitude","Long_",colnames(y))
y$Active <- NA
y$Admin2 <- NA
y$FIPS <- NA
y$Combined_Key <- NA
y$Case_Fatality_Ratio <- NA
y$Incident_Rate <- NA
} else if(fileDate < mdy("05/29/2020")){
y$Case_Fatality_Ratio <- NA
y$Incident_Rate <- NA
}
else if (fileDate <= mdy("11/09/2020")){
colnames(y) <- sub("Case.Fatality_Ratio","Case_Fatality_Ratio",colnames(y))
colnames(y) <- sub("Incidence_Rate","Incident_Rate",colnames(y))
}
# extract date from file name, assign to date column because
# Last_Update field format varies day by day and a good programmer is
# a lazy programmer
y$date <- substr(filenames[x],1,10)
y
})
# check number of columns, should be 15 for all files
table(unlist(lapply(1:length(theFiles),function(x) length(names(dataList[[x]])))))
data <- do.call(rbind,dataList)
# make directory & save the RDS
if(!dir.exists("./rdsData")) dir.create("./rdsData")
saveRDS(data,"./rdsData/covidData.rds")
#subset by date
system.time(data <- readRDS("./rdsData/covidData.rds"))
library(lubridate)
data$date <- mdy(data$date)
View(data)
View(data)
library(quantmod)
symbols = c('BTC-USD')
start = as.Date("2020-01-01")
until = as.Date("2022-08-26")
#df <- getSymbols('BTC-USD',src='yahoo', from = start, to = until,
#                  auto.assign = FALSE)
stocks = lapply(symbols, function(symbol) {
aStock = as.data.frame(getSymbols(symbol,src='yahoo', from = start, to = until,
auto.assign = FALSE))
colnames(aStock) <- c("Open","High","Low","Close","Volume","Adjusted")
aStock$Symbol <- symbol
aStock$Date <- as.Date(rownames(aStock),"%Y-%m-%d")
aStock
})
btcData <- do.call(rbind,stocks)
system.time(data <- readRDS("./rdsData/covidData.rds"))
library(tidyverse)
library(lubridate)
data$date <- mdy(data$date)
###Checking how many countries
countries <- as.data.frame(table(data$Country_Region))
subsetCovidData <- data %>%
filter(date > mdy("01/01/2020") & date < mdy("08/26/2022")) %>%
rename(Date = date) %>%
mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
Active = ifelse(is.na(Active),0,Active),
Deaths = ifelse(is.na(Deaths),0,Deaths),
Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region),
Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
# code to summarise() up to one row per country per day
group_by(Country_Region,Date) %>%
summarise(Confirmed = sum(Confirmed),
Active = sum(Active),
Deaths = sum(Deaths),
Recovered = sum(Recovered)) %>%
full_join(.,btcData)
saveRDS(subsetCovidData,"./rdsData/covidBitcoinData.rds")
adataframe <- readRDS("./rdsData/covidBitcoinData.rds")
#checking relation
identical(subsetCovidData,adataframe)
mut <- mutate(subsetCovidData,Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region))
EconData <- subsetCovidData %>%
filter(Country_Region %in% c("US","China","Ukraine","Russia","Japan",
"Germany","United Kingdom","France"))
View(EconData)
##Retrieve the dimension of object#
dim(EconData)
summary(EconData)
source("C:/Users/alsto/gitrepos/COVID-19/R/readCovidRDS.R")
plot(EconData$Date, EconData$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm")
#looking a column Date#
str(EconData$Date)
#looking a column Date#
str(EconData2$Date)
#convert into date format
x <- as.Date(EconData$Date)
head(x)
class(x)
str(x)
#create year, month, day column
year <-as.numeric(format(x,'%Y'))
head(year)
month <- as.numeric(format(x,'%m'))
head(month)
day <- as.numeric(format(x,'%d'))
head(day)
head(EconData)
##Adding the columns to the dataframe
EconData1 <- cbind(EconData,year,month,day)
head(EconData1)
View(EconData1)
##Adding the columns to the dataframe
EconData <- cbind(EconData,year,month,day)
#looking a column Date#
## not in a date time format
str(EconData$Date)
#convert into date format
x <- as.Date(EconData$Date)
head(x)
class(x)
str(x)
#create year, month, day column
year <-as.numeric(format(x,'%Y'))
head(year)
month <- as.numeric(format(x,'%m'))
head(month)
day <- as.numeric(format(x,'%d'))
head(day)
head(EconData)
##Adding the columns to the dataframe
EconData <- cbind(EconData,year,month,day)
head(EconData)
View(EconData)
##Retrieve the dimension of object#
dim(EconData)
subsetCovidData <- data %>%
filter(date > mdy("01/01/2020") & date < mdy("08/26/2022")) %>%
rename(Date = date) %>%
mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
Active = ifelse(is.na(Active),0,Active),
Deaths = ifelse(is.na(Deaths),0,Deaths),
Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region),
Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
# code to summarise() up to one row per country per day
group_by(Country_Region,Date) %>%
summarise(Confirmed = sum(Confirmed),
Active = sum(Active),
Deaths = sum(Deaths),
Recovered = sum(Recovered)) %>%
full_join(.,btcData)
system.time(data <- readRDS("./rdsData/covidData.rds"))
library(tidyverse)
library(lubridate)
data$date <- mdy(data$date)
###Checking how many countries
countries <- as.data.frame(table(data$Country_Region))
subsetCovidData <- data %>%
filter(date > mdy("01/01/2020") & date < mdy("08/26/2022")) %>%
rename(Date = date) %>%
mutate(Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
Active = ifelse(is.na(Active),0,Active),
Deaths = ifelse(is.na(Deaths),0,Deaths),
Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region),
Recovered = ifelse(is.na(Recovered),0,Recovered)) %>%
# code to summarise() up to one row per country per day
group_by(Country_Region,Date) %>%
summarise(Confirmed = sum(Confirmed),
Active = sum(Active),
Deaths = sum(Deaths),
Recovered = sum(Recovered)) %>%
full_join(.,btcData)
saveRDS(subsetCovidData,"./rdsData/covidBitcoinData.rds")
adataframe <- readRDS("./rdsData/covidBitcoinData.rds")
#checking relation
identical(subsetCovidData,adataframe)
mut <- mutate(subsetCovidData,Country_Region = if_else(Country_Region=="Mainland China","China",Country_Region))
EconData <- subsetCovidData %>%
filter(Country_Region %in% c("US","China","Ukraine","Russia","Japan",
"Germany","United Kingdom","France"))
#looking a column Date#
## not in a date time format
str(EconData$Date)
#create year, month, day column
year <-as.numeric(format(x,'%Y'))
head(year)
month <- as.numeric(format(x,'%m'))
head(month)
day <- as.numeric(format(x,'%d'))
head(day)
head(EconData)
##Adding the columns to the dataframe
EconData1 <- cbind(EconData,year,month,day)
head(EconData1)
world<- EconData1 %>%
select(Date,Country_Region,Confirmed,Deaths,Symbol,Open,Close,...14,...15,...16)%>%
rename(...14 = years, ...15 = month, ...16 = day)
world<- EconData1 %>%
select(Date,Country_Region,Confirmed,Deaths,Symbol,Open,Close,...14,...15,...16)
View(world)
world<- EconData1 %>%
select(Date,Country_Region,Confirmed,Deaths,Symbol,Open,Close,...14,...15,...16)%>%
rename(...14 = year)
world<- EconData1 %>%
select(Date,Country_Region,Confirmed,Deaths,Symbol,Open,Close,...14,...15,...16)%>%
rename(...14 = yearss)
x <- as.Date(EconData$Date)
head(x)
class(x)
str(x)
#create year, month, day column
year <-as.numeric(format(x,'%Y'))
head(year)
month <- as.numeric(format(x,'%m'))
head(month)
day <- as.numeric(format(x,'%d'))
head(day)
head(EconData)
##Adding the columns to the dataframe
#?
E <- cbind(EconData,year,month,day)
plot(world$...14, world$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm")
plot(world$...14, world$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm",lty=1,ylim=c(0,50000000))
plot(world[,3],xlab = "year",ylab = "Confirmed",type="l",lwd=2,
xlim=c(2020,2022))
plot(world[,3],xlab = "year",ylab = "Confirmed",type="l",lwd=2,
xlim=c(2020,2022)ylim=c(0,4000000))
plot(world[,3],xlab = "year",ylab = "Confirmed",type="l",lwd=2,
xlim=c(2020,2022),ylim=c(0,4000000))
plot(world[,4],xlab = "year",ylab = "Confirmed",type="l",lwd=2,
xlim=c(2020,2022),ylim=c(0,4000000))
plot(world[,4],xlab = "year",ylab = "Confirmed",type="l",lwd=2,col ="blue")
par(mfow=c(1,1))
plot(world[,3])
#convert into date format
x <- as.Date(EconData$Date)
head(x)
class(x)
str(x)
#create year, month, day column
year <-as.numeric(format(x,'%Y'))
head(year)
month <- as.numeric(format(x,'%m'))
head(month)
day <- as.numeric(format(x,'%d'))
head(day)
head(EconData)
##Adding the columns to the dataframe
#?
A <- cbind(EconData,year,month,date)
##Adding the columns to the dataframe
#?
A <- cbind(EconData,year,month,day)
plot(EconData$Date, EconData$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm")
View(A)
plot(world$...14, world$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm")
plot(world$...14, world$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm",lty=1,ylim=c(0,50000000))
par(mfow=c(1,1))
plot(A[,3])
install.packages("ggplot2")
install.packages("ggplot2")
library(ggplot2)
View(data)
View(data)
View(data)
plot(world$...15, world$Confirmed,type="l", xlab = "Month" ,
ylab = "Confirm")
ggplot2(data = A ,aes(x= ...15, y= Confirmed,group=1))+geom_line(linetype="dashed")+geom_point()
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
library(ggplot2)
#option 1
ggplot2(data = A ,aes(x= ...15, y= Confirmed,group=1))+geom_line(linetype="dashed")+geom_point()
library(ggplot2)
#option 1
ggplot(data = A ,aes(x= ...15, y= Confirmed,group=1))+geom_line(linetype="dashed")+geom_point()
#option 1
ggplot(data = A ,aes(x= ...14, y= Confirmed,group=1))+geom_line(linetype="dashed")+geom_point()
View(A)
ggplot(data = A, mapping = aes(x=...14,y=Close,col = "red"))+geom_poimt()
ggplot(data = A, mapping = aes(x=...14,y=Close,col = "red"))+geom_point()
library(fpp)
install.packages("fpp")
library(fpp)
seasonplot(A$Close,ylab="BTC Close",xlab="Year",
main="Season Plot: Bitcoin year data" ,
year.labels=TRUE, year.labels.left=TRUE, col=1:6, pch=19)
seasonplot(A$Close,A$...15,ylab="BTC Close",xlab="Year",
main="Season Plot: Bitcoin year data" ,
year.labels=TRUE, year.labels.left=TRUE, col=1:6, pch=19)
View(EconData)
View(EconData)
bit <-ts(EconData[,3],start=c(2020,1),frequency = 12)
bit <-ts(EconData[,3],start=c(2020,1),frequency = 12)
#########
#Preliminary Analysis
########
autoplot(bit)+
ggtitle("Time Plot: Bitcoin time data")+
xlab("Year")+
ylab("Covid")
bit <-ts(EconData[,3],start=c(2020,1),end = c(2030,12),frequency = 12)
#########
#Preliminary Analysis
########
autoplot(bit)+
ggtitle("Time Plot: Bitcoin time data")+
xlab("Year")+
ylab("Covid")
ggseasonplot(bit)+
ggtitle("Seasonal Plot: Date in Confirmed cases Covid-19")+
ylab("Confirmed Cases")+
xlab("Year")
bit <-ts(EconData[,3],start=c(2020,1),end = c(2025,12),frequency = 12)
ggseasonplot(bit)+
ggtitle("Seasonal Plot: Date in Confirmed cases Covid-19")+
ylab("Confirmed Cases")+
xlab("Year")
btcCose <-ts(EconData[,10],start=c(2020,1),end = c(2025,12),frequency = 12)
#Series appears trends-stationary
ggseasonplot(btcClose)+
ggtitle("Seasonal Plot: Date in Closed BTC ")+
ylab("Close Price")+
xlab("Year")
btcClose <-ts(EconData[,10],start=c(2020,1),end = c(2025,12),frequency = 12)
#Series appears trends-stationary
ggseasonplot(btcClose)+
ggtitle("Seasonal Plot: Date in Closed BTC ")+
ylab("Close Price")+
xlab("Year")
CovDeaths <-ts(EconData[,5],start=c(2020,1),end = c(2025,12),frequency = 12)
#Series appears trends-stationary
ggseasonplot(CovDeaths)+
ggtitle("Seasonal Plot: Date in Covid-19 Death Rates ")+
ylab("Death Rates")+
xlab("Year")
##Retrieve the dimension of object#
dim(EconData)
View(A)
plot(EconData$Date, EconData$Confirmed,type="l", xlab = "Year" ,
ylab = "Confirm")
ggplot(data = A ,aes(x= ...14, y= Confirmed,group=1))+geom_line(linetype="dashed")+geom_point()
btcClose <-ts(EconData[,10],start=c(2020,1),end = c(2025,12),frequency = 12)
#Series appears trends-stationary
ggseasonplot(btcClose)+
ggtitle("Seasonal Plot: Date in Closed BTC ")+
ylab("Close Price")+
xlab("Year")
btcClose <-ts(EconData[,10],start=c(2020,1),end = c(2022,12),frequency = 12)
#Series appears trends-stationary
ggseasonplot(btcClose)+
ggtitle("Seasonal Plot: Date in Closed BTC ")+
ylab("Close Price")+
xlab("Year")
source("C:/Users/alsto/gitrepos/COVID-19/R/TimeSeries.R")
source("C:/Users/alsto/gitrepos/COVID-19/R/TimeSeries.R")
source("C:/Users/alsto/gitrepos/COVID-19/R/TimeSeries.R")
source("C:/Users/alsto/gitrepos/COVID-19/R/TimeSeries.R")
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
##Boxplot
boxplot(EconData$Confirmed~EconData$Country_Region)
boxplot(EconData$Confirmed~EconData$Country_Region)+xlab("Countries")+
ylab("Covid Confirmed")
boxplot(EconData$Confirmed~EconData$Country_Region, xlab = "Year" ,
ylab = "Confirm")
boxplot(EconData$Confirmed~EconData$Country_Region, xlab = "Countries" ,
ylab = "Confirmed Covid")
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
boxplot(EconData$Confirmed~EconData$Country_Region, notch=T,
col=c("gold","gold","gold","gold","grey","grey","grey","grey"),
xlab = "Countries",ylab = "Confirmed Covid",
main= "COnfirmation for Covid-19 Cases")
source("C:/Users/alsto/gitrepos/COVID-19/R/readCovidRDS.R")
