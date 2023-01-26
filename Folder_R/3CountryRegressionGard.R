######## Choosing 3 Countries to Choose from-- From the GardWeekLubriate.R scrip
### RUN GARDWEEK
#### Final for time series
CasesData %>%
  filter(Country_Region %in% c("US","China","India")) %>%
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
         dailyDeaths = if_else(Date == mdy("01/22/2022"),Deaths,Deaths - lag(Deaths)))-> ThreeCountry


### Group AVG by Country_Region, Year, Week
##### Group the AVG by WEEK & YEAR.. Countries are NOT used.. Run to see by 
##### training data 2020-2021
#####test data 2022
ThreeWeek <- ThreeCountry %>%
  dplyr::group_by(year,week)%>%
  dplyr::summarize(DeathRate = mean(Deaths),
                   ConfirmedRate = mean(Confirmed),
                   CloseRate = mean(Close),
                   Incident_Rate = mean(Incident_Rate)) %>%
  
  ### Ungroup to create W_S as a time series to count the rows  
  ungroup() %>%
  mutate(week_squence = seq(1,n()))

######Variable analysis for the countries
boxplot(Deaths~Country_Region, data=ThreeCountry)

GardWeek2 <- ThreeCountry %>%
  dplyr::group_by(Country_Region, year,week)%>%
  dplyr::summarize(DeathRate = mean(Deaths),
                   ConfirmedRate = mean(Confirmed),
                   CloseRate = mean(Close))%>%
  dplyr::ungroup()

#### Previous group by.. Didnt need the Incident Rate
#GardWeek2 <- ThreeCountry %>%
#  dplyr::group_by(Country_Region, year,week)%>%
#  dplyr::summarize(DeathRate = mean(Deaths),
#                   ConfirmedRate = mean(Confirmed),
#                   CloseRate = mean(Close),
#                   Incident_Rate = mean(Incident_Rate))
### Comparing the 3 Countire
boxplot(DeathRate~Country_Region, data=GardWeek2, main= "3 Country Obvserve")
### Looking over the Regression analysis
LMThree<-lm(CloseRate~Country_Region + DeathRate + ConfirmedRate, data=GardWeek2)
summary(LMThree)



## Split data between train and test/ Validation
###First 776 rows will be used for training a multiple linear regression 
## training the data from the dates
### which date do I want to stop at?
#trainThree<- ThreeWeek[1:82,] ## copy the first 82 rows
#str(trainThree)

#last rows to 138
#testThree<- GardWeek2[83:138,] #Copy the last rows

# Run regression model with all the independent variables in the model
#BitTraining33<- lm(CloseRate~ year + DeathRate + Incident_Rate
#               + ConfirmedRate + week(), data=trainThree)
BitTraining33<- lm(CloseRate~., data=trainThree)
summary(BitTraining33)
#Run a stepAIC model
stepBitTrain33<-stepAIC(BitTraining33,direction="both")
## 47% of the variation of the variables
summary(stepBitTrain33)

############################ Validate with the test data
### predicting a model on the Bitcoin data
## ask the model to run by the test dataset
predictBitcoin2<-predict(stepBitTrain33,testThree)
predictBitcoin2

#Add predictBitcoin as a column to testdata
testThree["Predicted"] <- predictBitcoin2

View(testThree)
##Plot Actual and Predicted Bitcoin data
plot(testThree$Predicted,testThree$CloseRate,xlab = "Predicted",ylab = "Close Rate",
     main = "Predicting the model")

install.packages("forecast")
library(fpp2)
library(forecast)
library(tseries)
Time <- ts(ThreeWeek$CloseRate,start=c(2020),frequency=54, xlab= "Weeks",
           ylab= "Bitcoin Closing Rate")
autoplot(Time)

#####################################################################################

#make this example reproducible
set.seed(123)
####### For the FINAL MODEL (Train, and test)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(GardWeek2), replace=TRUE, prob=c(0.7,0.3))
trainS  <- GardWeek2[sample, ]
testS   <- GardWeek2[!sample, ]

library(caret)
  ### want all of the X
X_test<- testS%>%dplyr::select(- CloseRate)
### All the Y
Y_test<- testS%>%dplyr::select( CloseRate)
# Custom Control Parameters
Custom <- trainControl(method ="repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

#Linear Model
set.seed(1234)
l_m<-train(CloseRate~.,
          trainS,
          method = 'lm',
          trControl = Custom)
summary(l_m)
l_m$results
lm
plot(l_m$finalModel)


#Ridge Regression
## cross validation. the best for the model in lambda is 1
set.seed(1234)
ridge <- train(CloseRate~.,
               trainS,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.001,1,length=5)),
               trControl = Custom)
plot(ridge)
summary(ridge)
ridge


#Lasso Regression
#lambda = 1 means dont need to use   
set.seed(1234)
lasso <- train(CloseRate~.,
               trainS,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha=1,
                                      lambda = seq(0.0001,1,length=5)),
               trControl = Custom)
plot(lasso)

parameters <- c(seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 1))
##### DOnt need
###### Constructing the model for the paameters
lasso<-train(CloseRate~.,
             trainS,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 1, lambda = parameters) ,
             metric =  "Rsquared"
) 

ridge<-train(CloseRate~.,
             trainS,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             metric =  "Rsquared"
### Dont need             
) 
linear<-train(CloseRate~.,
              trainS, 
              method = 'lm',
              metric =  "Rsquared"
)
### the last parameter is 25
print(paste0('Lasso best parameters: ' , lasso$finalModel$lambdaOpt))
print(paste0('Ridge best parameters: ' , ridge$finalModel$lambdaOpt))

library(tidyverse)
#### compare the 3 to see the best fit
### comparing the models

predictions_lasso <- lasso %>% predict(X_test)
predictions_ridge <- ridge %>% predict(X_test)
predictions_lin <- linear %>% predict(X_test)

data.frame(
  Ridge_R2 = R2(predictions_ridge, Y_test),
  Lasso_R2 = R2(predictions_lasso, Y_test),
  Linear_R2 = R2(predictions_lin, Y_test))

###################################################################################



final.model<- data.frame(
  as.data.frame.matrix(coef(l_m$finalModel)),
  as.data.frame.matrix(coef(ridge$finalModel)),
  as.data.frame.matrix(coef(lasso$finalModel))
) %>%
  rename(Linear_coef = X1, Ridge_coef = X1.1, Lasso_coef = X1.2)
