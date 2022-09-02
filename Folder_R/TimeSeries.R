##Retrieve the dimension of object#
dim(EconData)
summary(EconData)

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
#?
A <- cbind(EconData,year,month,day)
E <- cbind(EconData,year,month,day)
EconData1 <- cbind(EconData,year,month,day)
head(EconData1)
#?
world<- EconData1 %>%
  select(Date,Country_Region,Confirmed,Deaths,Symbol,Open,Close,...14,...15,...16) 
#%>%
#  rename(...14 = year)
#######   Look up get names type of command in R: Youtube####  

## TIME SERIES##

plot(EconData$Date, EconData$Confirmed,type="l", xlab = "Year" ,
     ylab = "Confirm")

plot(world$...15, world$Confirmed,type="l", xlab = "Month" ,
     ylab = "Confirm")

plot(world$...14, world$Confirmed,type="l", xlab = "Year" ,
     ylab = "Confirm",lty=1,ylim=c(0,50000000))

#for 1 plot
#par(mfrow=c(1,1))
#plot(A[,3])
#plot(world[,4],xlab = "year",ylab = "Confirmed",type="l",lwd=2,col ="blue")

install.packages("ggplot2")
library(ggplot2)

#option 1
ggplot(data = A ,aes(x= ...14, y= Confirmed,group=1))+geom_line(linetype="dashed")+geom_point()
ggplot(data = A, mapping = aes(x=...14,y=Close,col = "red"))+geom_point()

install.packages("fpp")
library(fpp)

seasonplot(A$Close,A$...15,ylab="BTC Close",xlab="Year",
           main="Season Plot: Bitcoin year data" ,
           year.labels=TRUE, year.labels.left=TRUE, col=1:6, pch=19)



#Decare this as time series data
#### Covid-19 Confirmed Cases
Cov1 <-ts(EconData[,3],start=c(2020,1),end = c(2025,12),frequency = 12)

#########
#Preliminary Analysis
########
autoplot(Cov1)+
  ggtitle("Time Plot: Covid-19 Comfirmed")+
  xlab("Year")+
  ylab("Covid")


#Series appears trends-stationary
ggseasonplot(Cov1)+
  ggtitle("Seasonal Plot: Date in Confirmed cases Covid-19")+
  ylab("Confirmed Cases")+
  xlab("Year")

##### Bitcoin Closing prices
btcClose <-ts(EconData[,10],start=c(2020,1),end = c(2022,12),frequency = 12)

#Series appears trends-stationary
ggseasonplot(btcClose)+
  ggtitle("Seasonal Plot: Date in Closed BTC ")+
  ylab("Close Price")+
  xlab("Year")

###### Covide-19 Death series
CovDeaths <-ts(EconData[,5],start=c(2020,1),end = c(2025,12),frequency = 12)

#Series appears trends-stationary
ggseasonplot(CovDeaths)+
  ggtitle("Seasonal Plot: Date in Covid-19 Death Rates ")+
  ylab("Death Rates")+
  xlab("Year")

