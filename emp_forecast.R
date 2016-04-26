#employment forecast project

#packages
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

#load data in
x.emp <- getURL("https://raw.githubusercontent.com/brndngrhm/employment/master/emp.csv")
emp <- as.data.frame(read.csv(text = x.emp, strip.white = T))
emp <- na.omit(emp)
emp <- melt(emp, id.vars = "Year")
names(emp) <- tolower(names(emp))
names(emp)[2] <- "month"
names(emp)[3] <- "jobs"
emp$month <- month(emp$month, label=T)
emp$year <- ymd(emp$year)
jobs <- ts(emp[3], frequency = 12)

#plot
plot(jobs, type="o")
acf2(jobs)






