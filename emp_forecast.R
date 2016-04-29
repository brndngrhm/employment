#employment forecast project

#packages ----
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(RCurl)
library(rvest)
library(ggplot2)
library(scales)
library(astsa)
library(ggthemes)
library(extrafont)
library(reshape)
library(car)
library(forecast)
library(fGarch)

#load data in ----
x.emp <- getURL("https://raw.githubusercontent.com/brndngrhm/employment/master/emp.csv")
emp <- as.data.frame(read.csv(text = x.emp, strip.white = T))
emp <- na.omit(emp)
emp <- melt(emp, id.vars = "Year")
names(emp) <- tolower(names(emp))
names(emp)[2] <- "month"
names(emp)[3] <- "jobs"
emp$month2[emp$month == "Jan"] <- 1
emp$month2[emp$month == "Feb"] <- 2
emp$month2[emp$month == "Mar"] <- 3
emp$month2[emp$month == "Apr"] <- 4
emp$month2[emp$month == "May"] <- 5
emp$month2[emp$month == "Jun"] <- 6
emp$month2[emp$month == "Jul"] <- 7
emp$month2[emp$month == "Aug"] <- 8
emp$month2[emp$month == "Sep"] <- 9
emp$month2[emp$month == "Oct"] <- 10
emp$month2[emp$month == "Nov"] <- 11
emp$month2[emp$month == "Dec"] <- 12
emp <- emp %>% arrange(year, month2)
emp$month2 <- NULL

jobs <- ts(emp[3], frequency = 12)

#plot ----
plot(jobs, type="l")
plot(log(jobs), type="l")
acf2(jobs)

diff.jobs <- diff(log(jobs))
plot(diff.jobs, type="l")
acf2(diff.jobs)

dl12.jobs <- diff(diff.jobs,12)
plot(dl12.jobs, type="l")
acf2(dl12.jobs, max.lag = 80)

#auto-arima ----
model1 <- auto.arima(log(jobs))
arima.fit <- ts(fitted(arima(log(jobs), order=c(1, 2, 2), seasonal = c(0, 1, 1))), frequency = 12)
auto.arima.forecast <- sarima.for(log(jobs),6 ,1,2,2,0,1,1, 12) #6 month forecast

#regresion with arma errors ----
jobs2 <- log(emp$jobs)
month2 <- emp$month
t <- seq(1:924)
t2 <- emp$t^2


lm <- lm(jobs2[2:924] ~ t[1:923] + jobs2[1:923] + factor(month2[1:923]))
summary(lm)

resids <- lm$residuals
plot(resids, type="l")
acf2(resids)

uplim=4
aicmat.resids <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids[i+1,j+1]=sarima(resids,0,1,0,i,1,j,12,details=F,tol=0.001)$AIC}}

print(aicmat.resids)

aicmat.resids2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids2[i+1,j+1]<-sarima(resids, i, 1, j, 1, 1, 3, 12, details=F, tol=0.001)$AIC}}

print(aicmat.resids2)

#resids model
sarima(resids,1,1,1,1,1,3,12,details=F)

#doing the forecat

regressors <- cbind(t[1:923], jobs2[1:923], factor(month2[1:923]))
future.time <- c(924)
future.jobs2 <- c(11.87837)
future.month <- factor("Jan")
future.vals <- cbind(future.time, future.jobs2, future.month)

fit <- Arima(jobs2[2:924],order=c(1,1,1), seasonal = c(1,1,3), xreg=regressors)

fcast <- forecast(fit, h=1, xreg=future.vals)

reg.arma.fit <- fitted(fit)

#plotting stuff ----

#setting plot parameters
n <- length(jobs)
time <- seq(1:n)
auto.arima.time <- seq(n+1, n+6)
auto.arima.pred <- auto.arima.forecast$pred

#base plot
plot(time, log(jobs), type = "p", xlim=c(n-60, n+10))
lines(time, arima.fit, col="red", type ="l")

#adding auto.arima forecast points and lines
points(n+1, auto.arima.forecast$pred[1])
points(n+2, auto.arima.forecast$pred[2])
points(n+3, auto.arima.forecast$pred[3])
points(n+4, auto.arima.forecast$pred[4])
points(n+5, auto.arima.forecast$pred[5])
points(n+6, auto.arima.forecast$pred[6])
lines(auto.arima.time, auto.arima.pred, type="l", col = "blue")

#actual forecast
forecast <- data.frame(c(exp(auto.arima.forecast$pred[1]), exp(auto.arima.forecast$pred[2]),
                         exp(auto.arima.forecast$pred[3]), exp(auto.arima.forecast$pred[4]),
                         exp(auto.arima.forecast$pred[5]), exp(auto.arima.forecast$pred[6])))
names(forecast)[1] <- "jobs"
