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

#jobs data from http://www.bls.gov/news.release/empsit.nr0.htm ----
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

emp.ts <- ts(emp$jobs, frequency = 12)
#emp.ts <- jobs[1:195]
#emp.ts<- ts(emp.ts, frequency = 12)
emp.change <- diff(emp.ts)
date2 <- emp$date[1:195]

#sp 500 data from https://finance.yahoo.com/q/hp?s=%5EGSPC&a=00&b=1&c=2000&d=03&e=1&f=2016&g=m ----
x.sp <- getURL("https://raw.githubusercontent.com/brndngrhm/employment/master/sp_500.csv")
sp <- as.data.frame(read.csv(text = x.sp, strip.white = T))
names(sp) <- tolower(names(sp))
sp <- sp %>% select(date, adj.close)
sp$date <- ydm(sp$date)
sp$date <- ymd(sp$date)
sp$month <- month(sp$date, label = T)
sp$year <- year(sp$date)
sp <- sp %>% select(date, year, month, adj.close)

sp.ts <- ts(sp$adj.close, frequency = 12)
#sp.ts <- sp.ts[1:195]
#sp.ts<- ts(sp.ts, frequency = 12)
sp.change <- diff(sp.ts)

#manufacturing data from https://www.philadelphiafed.org/research-and-data/regional-economy/business-outlook-survey/historical-data ----
#definitions from https://www.philadelphiafed.org/-/media/research-and-data/regional-economy/business-outlook-survey/readme.txt?la=en
#want the column titled gacdfna - "general activity index"
#this pulls directly from website, dont need to manually download to update

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

manuf.ts <- ts(manuf$gacdfna, frequency = 12)
#manuf.ts <- manuf.ts[1:195]
#manuf.ts<- ts(manuf.ts, frequency = 12)
manuf.change <- diff(manuf.ts)

#plots ----

#jobs
plot(emp.ts, type="l")
acf2(emp.ts)

plot(date2, emp.change, type="l")
acf2(emp.change)

diff.emp.ts <- diff(emp.change)
plot(diff.emp.ts, type="l")
acf2(diff.emp.ts)

#manuf
plot(manuf.ts, type="l")
acf2(manuf.ts)

plot(date2, manuf.change, type="l")
acf2(emp.change)

diff.manuf.ts <- diff(manuf.change)
plot(diff.manuf.ts, type="l")
acf2(diff.manuf.ts)

#s&p
plot(sp.ts, type="l")
acf2(sp.ts)

plot(date2, sp.change, type="l")
acf2(sp.change)

diff.sp.ts <- diff(sp.change)
plot(diff.sp.ts, type="l")
acf2(diff.sp.ts)

#auto-arima ----
(model1 <- auto.arima(emp.change))
arima.fit <- ts(fitted(arima(emp.change, order=c(0,1,1), seasonal = c(2,0,0)), frequency = 12))
auto.arima.forecast <- sarima.for(emp.change,1,1,0,0,0,0,0,12) #1 month forecast
auto.arima.forecast$pred  

#sarima ----
uplim=4
aicmat <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat[i+1,j+1]=sarima(emp.change,0,1,0,i,1,j,12,details=F,tol=0.001)$AIC}}

print(aicmat)

aicmat2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat2[i+1,j+1]<-sarima(emp.change, i, 1, j, 4, 1, 1, 12, details=F, tol=0.001)$AIC}}

print(aicmat2)

(sarima(emp.change,0,1,1,4,1,1,12,details = F))

(sarima <- sarima.for(emp.change,1,0,1,1,4,1,1,12))

#sarima fit
sarima.fit <- ts(fitted(arima(emp.change, order=c(0, 1, 1), seasonal = c(4, 1, 1))), frequency = 12)

#regresion with arma errors ----
#download different things like monthly gdp, income, manufacturing, stock market ...
#and check ccf with change in jobs to see if any could be leading indicators

manuf.ccf <- ccf(manuf.change, emp.change) #2,4,6,9,12,14,16 months
sp.ccf <- ccf(sp.change, emp.change) #6,11,16,19 months

month2 <- emp$month
t <- seq(1:194)
t2 <- t^2

lm <- lm(emp.change[21:195] ~ t[20:194] + t2[20:194] + emp.change[20:194] + 
           manuf.change[19:193] + manuf.change[17:191] + manuf.change[15:189] + manuf.change[12:186] + 
           manuf.change[9:183] + manuf.change[7:181] + manuf.change[5:179] + 
           sp.change[15:189] + sp.change[10:184] + sp.change[5:179] + sp.change[2:176] + 
           factor(month2[20:194]))
summary(lm)

step(lm, direction='forward')
step(lm, direction='backward')
step(lm, direction='both')

lm2 <- lm(emp.change[21:195] ~ t2[20:194] + emp.change[20:194] + 
           manuf.change[15:189] + manuf.change[5:179] + sp.change[2:176])

summary(lm2)

resids <- lm2$residuals
plot(resids, type="l")
acf2(resids)
acf2(diff(resids))

uplim=4
aicmat.resids <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids[i+1,j+1]=sarima(resids,0,1,0,i,0,j,12,details=F,tol=0.001)$AIC}}

print(aicmat.resids)

aicmat.resids2 <- matrix(double((uplim+1)^2),uplim+1,uplim+1)
for (i in 0:uplim){
  for (j in 0:uplim){
    aicmat.resids2[i+1,j+1]<-sarima(resids, i, 1, j, 1, 0, 0, 12, details=F, tol=0.001)$AIC}}

print(aicmat.resids2)

#resids model
sarima(resids,1,1,1,0,0,0,12,details=F)

#doing the forecat
regressors <- cbind(emp.change[20:194], manuf.change[15:189], manuf.change[5:179], sp.change[2:176])
future.emp.change <- emp.change[195]
future.manuf1 <- manuf.change[190]
future.manuf2 <- manuf.change[180]
future.sp <- sp.change[177]
future.vals <- cbind(future.emp.change, future.manuf1, future.manuf2, future.sp)

fit <- Arima(emp.change[21:195],order=c(1,1,1), xreg=regressors)

fcast <- forecast(fit, h=1, xreg=future.vals)

reg.arma.fit <- fitted(fit)

#plotting stuff ----

#setting plot parameters
n <- length(emp.change)
time <- seq(1:n)
auto.arima.time <- n+1
sarima.time <- n+1
auto.arima.pred <- auto.arima.forecast$pred
sarima.pred <- sarima$pred
reg.arma.pred <- fcast

#base plot, checking fit
plot(time, emp.change, type = "p")

#adding auto-arima fit
lines(time, arima.fit, col="red", type ="l")

#adding sarima fit
lines(time, sarima.fit, col="blue", type ="l")

#adding reg.arma errors fit
lines(time[21:195], reg.arma.fit, col="green", type="l")



#base plot for adding forecast points
plot(time, emp.change, type = "p", xlim=c(n-10, n+10))

#adding auto-arima fit
lines(time, arima.fit, col="red", type ="l")

#adding sarima fit
lines(time, sarima.fit, col="blue", type ="l")

#adding reg.arma errors fit
lines(time[21:195], reg.arma.fit, col="green", type="l")

#adding auto.arima forecast points and lines
points(n+1, auto.arima.pred, col = "red")
text(n+2, auto.arima.pred, "182", col="red")

#adding sarima forecast points and lines
points(n+1, sarima$pred, col = "blue")
text(n+1, sarima$pred+55, "217", col="blue")

#adding sarima forecast points and lines
points(n+1, 196.9909, col = "green")
text(n+2, 196.9909, "196", col="green")

#scomposite fcast = 198.3333

