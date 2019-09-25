##Problem 1##
edu<-c(10,12,12,16,16)
t.test(edu, mu=16, conf.level=.9)
t.test(edu, mu=16, conf.level=.95)


##Problem 2##
prop.test(154,200,.7,alternative="greater", correct=FALSE)
prop.test(154,200,.7,alternative="greater", correct=FALSE, conf.level=.99)

##Problem 3##
setwd("~/Desktop/POL 51 R Folder")
speed<-read.csv("driving.csv")
attach(speed)
t.test(speed, mu=90, alternative="greater", conf.level=.99)

##Problem 4##
ecn<-c(45,65,35,45,85)
are<-c(20,55,40,40,20)
t.test(ecn,are,var.equal = TRUE, mu=0)

##Problem 5##
years.of.education<-c(16,14,18,12,20)
earnings<-c(35,15,40,30,55)
cor.test(earnings,years.of.education)
qt(.95,3)
qt(.99,3)
install.packages("stargazer")
library(stargazer)
regmod1<-lm(earnings~years.of.education)
summary(regmod1)
stargazer(regmod1, type="text")
confint(regmod1, level = .9)
confint(regmod1, level = .95)
qt(.9,3)
qt(.95,3)

##Problem 6##
heights<-read.csv("galton.csv")
regmod2<-lm(father~son, data=heights)
summary(regmod2)
stargazer(regmod2, type="text")
