library(foreign)

setwd("/Users/bradfordjones/Dropbox/perspective taking and dehumanization/2017 Border Policy Study/Analysis Files")

dataclass<-read.dta("border_frame.dta")

attach(dataclass)

#Innumeracy Item: 
#Of the total Hispanic population living in the U.S. 
#(that is, immigrants and citizens), what percentage 
#of this population do you think are undocumented 
#immigrants? We are just interested in your best guess.

#Summarize the data: 
summary(undocumented_size_1)
#Summarize the data by gender
#Males:
mean(subset(dataclass, gender ==1)$undocumented_size_1, na.rm=TRUE)
#Females:
mean(subset(dataclass, gender ==2)$undocumented_size_1, na.rm=TRUE)

par(mfrow=c(2,2))
#Visualizing the data: BOX PLOTS

#Boxplots
boxplot(undocumented_size_1,  
        ylab=c("Estimate of Size"), 
        ylim=c(0,100), 
        main="Boxplot of undocumented pop. estimate", 
        col="skyblue3")
abline(h=16.7, lty=2, col="black")


#Dotchart: Sorted and made visually more appealing
dotchart(sort(undocumented_size_1),cex=.7,
         xlab="Estimated Size (in percentages)",
         pch=20, col="red",
         main="People overestimate size of \nundocumented Hispanic population",
         lcolor="transparent")
abline(v=16.7, lty=2, col="black")

#Index Plot: Sorted and made visually more appealing
plot(sort(undocumented_size_1),cex=.7,
         xlab="Index Number",
         ylab="Estimated Size (in percentages)",
         pch=20, col="red",
         main="People overestimate size of \nundocumented Hispanic population"
         )
abline(h=16.7, lty=2, col="black")

#Side-by-Side Boxplot
#Giving Labels for Gender variable
gender[gender==1] <- "Male"
gender[gender==2] <- "Female"

boxplot(undocumented_size_1~gender, 
        ylim=c(0,100), 
        ylab="Estimated Size (in percentages)",
        main="Female respondents have higher \ndegree of liberal self-placement",
        col=c("skyblue2", "orange"))
abline(h=16.7, lty=2, col="black")

#One-group t-test

t.test(undocumented_size_1, 
       mu=16.7,
       alternative="greater")


#Comparison of two groups: 

#First summarize the data by gender: 
median(subset(dataclass, gender ==1)$undocumented_size_1, na.rm=TRUE)
median(subset(dataclass, gender ==2)$undocumented_size_1, na.rm=TRUE)
mean(subset(dataclass, gender ==1)$undocumented_size_1, na.rm=TRUE)
mean(subset(dataclass, gender ==2)$undocumented_size_1, na.rm=TRUE)
sd(subset(dataclass, gender ==1)$undocumented_size_1, na.rm=TRUE)
sd(subset(dataclass, gender ==2)$undocumented_size_1, na.rm=TRUE)
IQR(subset(dataclass, gender ==1)$undocumented_size_1, na.rm=TRUE)
IQR(subset(dataclass, gender ==2)$undocumented_size_1, na.rm=TRUE)



#Two-group t-test

t.test(undocumented_size_1~gender,
       mu=0,
       alternative="less")
