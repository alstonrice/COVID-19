

library(readr)
pisa <- read_csv("C:/Users/alsto/Downloads/pisa.csv")
View(pisa)


mod <- lm(MathMean~ ReadingMean, data=pisa)
mod


###### MATH STATS. Final
### Question 1
##n=16
pnorm(1.2)
### calculate the left tail and to get both sides of the tail
1-pnorm(-1.2)*2
#n=20
pnorm(1.3)
1-pnorm(-1.34)*2
#n=30
pnorm(1.64)
1-pnorm(1.64)*2
#n=40
pnorm(1.9)
1-pnorm(-1.9)*2
#n=60
pnorm(2.32)
1-pnorm(-2.32)*2


###Question 2
punif(7,min=0,max=20)


wait_monday <- runif(8000, min = 0, max = 20)
wait_wednesday <- runif(8000, min = 0, max = 20)
wait_friday <- runif(8000, min = 0, max = 20)
longest_wait <- pmax(wait_monday, 
                     wait_wednesday,
                     wait_friday)
hist(longest_wait)
mean(longest_wait < 7)
mean(longest_wait)
sd(longest_wait)

####Question3
### perameterization (Hyper-perameters)
onesamplemean <- function(n){
  genders <- c(1,0)
  proportion <- c(0.6,0.4)
  s <- sample(genders, size = n, prob = proportion, replace = TRUE)
  m <- mean(s)
  return(m)
}
Means100<- replicate(100, onesamplemean(100))
hist(Means100)
Means1000<- replicate(1000, onesamplemean(100))
hist(Means1000)
Means10000<- replicate(10000, onesamplemean(100))
hist(Means10000)
Means300<- replicate(100, onesamplemean(300))
hist(Means300)
Means3000<- replicate(1000, onesamplemean(300))
hist(Means3000)
Means30000<- replicate(10000, onesamplemean(300))
hist(Means30000)
###Question4
#(B)
Rdata<-rpois(20,25)
print(Rdata)
## sample mean of the data for the Poisson Dist.
print(mean(Rdata))
#(C) finding calculatuation of Lambda
llikelHood <- function (lambda,data){
  -length(data)*lambda+log(lambda)*sum(data)-sum(log(factorial(data)))
}
result <- optimize(llikelHood, c(0,10^6), maximum = T, data=Rdata)
print(result$maximum)
####Question5
library(tidyverse)

set.seed(34567) #fixes the seed for the random generator
x <- rnorm(n = 100)
eps <- rnorm(n = 100)
yLin <- 0.6 + 1.2 * x + eps
####Calculate response Y
print(yLin)
leastSquares <- data.frame(x = x, yLin = yLin)
print(leastSquares)
#Convariance
Sxy<-cov(x,yLin)
Sxy
#Variance
Sx2 <-var(x)
Sx2
lm_fit <- lm(yLin~x)
lm_fit
d