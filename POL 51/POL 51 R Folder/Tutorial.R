library(foreign)

data_class<-read.csv("week1survey.csv")

attach(data_class)

summary(data_class)

mean(wall_slider_1, na.rm=TRUE)

hist(wall_slider_1,
     xlim=c(0,100),
     ylab="stanky danky meems",
     xlab="steel beems",
     col="green",
     main="Bush did 9/11",
     breaks=20)
     
median(wall_slider_1, na.rm=TRUE)

hist(wall_slider_1,
     xlim=c(0,100),
     ylab="Frequency",
     xlab="Level-of_support for border wall",
     col="hotpink1",
     main="Respondents are strongly opposed to border wall",
     breaks=20)

hist(wall_slider_1,
     xlim=c(0,100),
     ylab="stanky danky meems",
     xlab="steel beems",
     col="green",
     main="Bush did 9/11",
     breaks=20)
