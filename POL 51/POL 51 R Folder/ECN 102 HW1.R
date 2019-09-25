## Problem 2 - Descriptive Statistics ##


## Preliminary Commands ##
setwd("~/Desktop/ECN 102 HW#1")
install.packages("moments")
library(moments)
install.packages("modeest")
library(modeest)
mycols<-c("lightblue3","linen","lightcyan")
wage<-c(10,10,16,18,18,18)
summary(wage)


## 2A - Measures of Center ##
mean(wage)
exp(mean(log(wage)))
median(wage)
mfv(wage)

## 2B - Measures of Spread ##
var(wage)
sd(wage)
abs(sd(wage)/mean(wage))

## 2C - IQR ##
IQR(wage,type=6)

## 2D - Boxplot ##
boxplot(wage,
        cex = .7, cex.lab=.9,cex.axis=.9,
        ylab="Hourly Wages (in USD)", cex.ylab=1,
        main="Boxplot of Hourly Wages of \nWorkers within the Sample", cex.main = 1,
        col=c("lightblue1"))

## 2E - Pie Chart ##
wage_percent<-c(33.3,16.7,50)
lbl<-c("$10.00 (33.3%)","$16.00 (16.7%)","$18.00 (50.0%)")
pie(wage_percent,labels = lbl, main="Percentages of Various \nWages in the Sample", 
      cex.main = 1.2,
      col = mycols, font=1)

## 2F - Histogram ##
hist(wage,
     main="Histogram of Various Hourly \nWages Within the Sample",
     xlab="Hourly Wages (in USD)",
     border="black",
     col="lightblue3",
     xlim=c(10,18))

## 2G - Skewness ##
skewness(wage)

## 2H - Kurtosis ##
kurtosis(wage)
exkurt<-kurtosis(wage)-3

## FIN ##
    
