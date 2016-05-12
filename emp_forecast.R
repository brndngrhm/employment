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
#data from http://www.adpemploymentreport.com/
#and is # of employees in U.S. nonfarm private sector employment

#http://www.bls.gov/news.release/empsit.nr0.htm ----
#http://www.bls.gov/webapps/legacy/cesbtab1.htm
#use 2nd link to manually pull data each month, use seasonally adjusted, total nonfarm
x.emp <- getURL("https://raw.githubusercontent.com/brndngrhm/employment/master/emp.csv")
emp <- as.data.frame(read.csv(text = x.emp, strip.white = T))
emp$X <- NULL
names(emp)[2] <- 1
names(emp)[3] <- 2
names(emp)[4] <- 3
names(emp)[5] <- 4
names(emp)[6] <- 5
names(emp)[7] <- 6
names(emp)[8] <- 7
names(emp)[9] <- 8
names(emp)[10] <- 9
names(emp)[11] <- 10
names(emp)[12] <- 11
names(emp)[13] <- 12
names(emp) <- tolower(names(emp))
emp2 <- melt.data.frame(emp, id.vars = "year")
emp2 <- na.omit(emp2)
names(emp2)[2] <- "month"
names(emp2)[3] <- "jobs"
emp2$month <- as.numeric(emp2$month)
emp2$month2 <- emp2$month
emp2$month2[emp2$month < 10] <- paste("0", emp2$month, sep="")
emp2$date <- paste( emp2$year,emp2$month2, "01", sep="-" )
emp2$date2 <- ymd(emp2$date)
emp <- emp2
emp$year <- NULL
emp$month <- NULL
emp$month2 <- NULL
emp$date <- NULL
names(emp)[2] <- "date"
emp$year <- year(emp$date)
emp$month <- month(emp$date, label = T)
emp <- emp %>% select(date, year, month, jobs)
emp <- emp %>% arrange(date)

jobs <- ts(emp$jobs, frequency = 12)
jobs <- jobs[1:195]
jobs<- ts(jobs, frequency = 12)
change <- diff(jobs)
date2 <- emp$date[1:194]

#sp 500 data from https://finance.yahoo.com/q/hp?s=%5EGSPC&a=00&b=1&c=2000&d=03&e=1&f=2016&g=m ----



#manufacturing data from https://www.philadelphiafed.org/research-and-data/regional-economy/business-outlook-survey/historical-data ----
#definitions from https://www.philadelphiafed.org/-/media/research-and-data/regional-economy/business-outlook-survey/readme.txt?la=en
#want the column titled gacdfna - "general activity index"

#read in data
x.manuf <- getURL("https://www.philadelphiafed.org/-/media/research-and-data/regional-economy/business-outlook-survey/historical-data/data-series/bos_history.csv?la=en")
manuf <- as.data.frame(read.csv(text = x.manuf, strip.white = T))
names(manuf) <- tolower(names(manuf))
manuf <- manuf %>% select(date, gacdfna)

#Foratting the date 
date <- data.frame(manuf$date)
date <- separate(date, col="manuf.date", into = c("month", "year"), sep="-")
year <- as.numeric(date$year)
n <- length(year)
year2 <- year[1:380] 
year3 <- paste(19, year2, sep='')
year3 <- data.frame(year3)
year4 <- year[381:n]
year4.1 <- year4[1:120]
year4.1 <- paste(200, year4.1, sep='')
year4.1 <- data.frame(year4.1)
year4.2 <- year4[121:196]
year4.2 <- paste(20, year4.2, sep='')
year4.2 <- data.frame(year4.2)
names(year4.2)[1] <- "year4.1"
year4 <- rbind(year4.1, year4.2)
names(year4)[1] <- "year3"
year <- rbind(year3, year4)
date <- cbind(date, year)
date$month2 <- "01"
date$month2[date$month == "Feb"] <- "02"
date$month2[date$month == "Mar"] <- "03"
date$month2[date$month == "Apr"] <- "04"
date$month2[date$month == "May"] <- "05"
date$month2[date$month == "Jun"] <- "06"
date$month2[date$month == "Jul"] <- "07"
date$month2[date$month == "Aug"] <- "08"
date$month2[date$month == "Sep"] <- "09"
date$month2[date$month == "Oct"] <- "10"
date$month2[date$month == "Nov"] <- "11"
date$month2[date$month == "Dec"] <- "12"
date$date2 <- paste(date$month2, "01", date$year3, sep='-')
date$date <- paste(date$month, date$year, sep='-')
date$month <- NULL
date$year <- NULL
date$month2 <- NULL
date$year3 <- NULL
date$date <- as.factor(date$date)
manuf <- left_join(manuf, date, by = "date")
manuf$date <- NULL
names(manuf)[2] <- "date"
manuf$date <- mdy(manuf$date)
manuf$month <- month(manuf$date, label = T)
manuf$year <- year(manuf$date)
manuf <- manuf %>% dplyr::filter(year > 1999)
manuf <- manuf %>% select(date, year, month, gacdfna)
#this pulls directly from website, dont need to manually download to update
#plots ----
plot(jobs, type="l")
(jobs.plot <- ggplot(emp, aes(x=date, y=jobs)) + geom_line())
acf2(jobs)

plot(date2, change, type="l")
acf2(change)

diff.jobs <- diff(change)
plot(diff.jobs, type="l")
acf2(diff.jobs)

#auto-arima ----
(model1 <- auto.arima(change))
arima.fit <- ts(fitted(arima(change, order=c(0,1,1), seasonal = c(2,0,0)), frequency = 12))
auto.arima.forecast <- sarima.for(change,1,1,0,0,0,0,0,12) #1 month forecast
auto.arima.forecast$pred  

#sarima ----
uplim=4
aicmat <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat[i+1,j+1]=sarima(change,0,1,0,i,1,j,12,details=F,tol=0.001)$AIC}}

print(aicmat)

aicmat2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat2[i+1,j+1]<-sarima(change, i, 1, j, 3, 1, 1, 12, details=F, tol=0.001)$AIC}}

print(aicmat2)

(sarima(change,0,1,1,4,1,1,12,details = F))

(sarima <- sarima.for(change,1,0,1,1,4,1,1,12))

#sarima fit
sarima.fit <- ts(fitted(arima(change, order=c(0, 1, 1), seasonal = c(4, 1, 1))), frequency = 12)

#regresion with arma errors ----
#download different things like monthly gdp, income, manufacturing, stock market ...
#and check ccf with change in jobs to see if any could be leading indicators

month2 <- emp$month
t <- seq(1:194)
t2 <- emp$t^2


lm <- lm(change[2:194] ~ t[1:193] + change[1:193] + factor(month2[1:193]))
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
n <- length(change)
time <- seq(1:n)
auto.arima.time <- n+1
sarima.time <- n+1
auto.arima.pred <- auto.arima.forecast$pred
sarima.pred <- sarima$pred

#base plot, checking fit
plot(time, change, type = "p")

#adding auto-arima fit
lines(time, arima.fit, col="red", type ="l")

#adding sarima fit
lines(time, sarima.fit, col="blue", type ="l")

#base plot for adding forecast points
plot(time, change, type = "p", xlim=c(n-10, n+10))

#adding auto-arima fit
lines(time, arima.fit, col="red", type ="l")

#adding sarima fit
lines(time, sarima.fit, col="blue", type ="l")

#adding auto.arima forecast points and lines
points(n+1, auto.arima.pred, col = "red")
text(n+2, auto.arima.pred, "182", col="red")

#adding sarima forecast points and lines
points(n+1, sarima$pred, col = "blue")
text(n+1, sarima$pred+55, "217", col="blue")

#adding actual employment number forecast points and lines
points(n+1, 160, col = "green")
text(n+1, 120, "160", col="green")

