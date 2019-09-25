###Run everything from lines 2 to  15 before running rest of script###
setwd("~/Desktop/POL 51 R Folder")
mfs<-read.csv("pooled_data.csv", header =TRUE)

mfs$gender[mfs$gender==3] <-NA
mfs$gender[mfs$gender==5] <-NA
mfs$gender_binary[mfs$gender=="1"] <- 0
mfs$gender_binary[mfs$gender=="2"] <- 1
mfs$pid[mfs$pid==9] <- 1
mfs$pid[mfs$pid==10] <- 2
mfs$pid[mfs$pid==11] <- 3
mfs$pid[mfs$pid==12] <- 4
mfs$pid_binary[mfs$pid=="1"] <- 0
mfs$pid_binary[mfs$pid=="2"] <- 1

attach(mfs)


## Problem 1 ##
boxplot(hispanic_pop_1,
        main = " Estimates of Percentage of Hispanic Pop. 
        that is Undocumented in US", cex.main=.8,
        cex=.4,
        col = "coral",
        ylab = "Undocumented Estimate")
abline(h=16.7, lty = 2)


dotchart(sort(hispanic_pop_1),cex=.7,
         xlab="Estimated Size (in percentages)",
         color = 'cadetblue',
         main="Sorted Estimates of Undocumented 
         Hispanic Pop. in US",
         cex.main=1.4)
abline(v=16.7, lty = 2)


## Problem 2 ##
mean(subset(mfs, gender ==1)$hispanic_pop_1, na.rm=TRUE)
mean(subset(mfs, gender ==2)$hispanic_pop_1, na.rm=TRUE)
sd(subset(mfs, gender ==1)$hispanic_pop_1, na.rm=TRUE)
sd(subset(mfs, gender ==2)$hispanic_pop_1, na.rm=TRUE)
median(subset(mfs, gender ==1)$hispanic_pop_1, na.rm=TRUE)
median(subset(mfs, gender ==2)$hispanic_pop_1, na.rm=TRUE)
IQR(subset(mfs, gender ==1)$hispanic_pop_1, na.rm=TRUE)
IQR(subset(mfs, gender ==2)$hispanic_pop_1, na.rm=TRUE)


gender[gender==1] <- "Male"
gender[gender==2] <- "Female"
boxplot(hispanic_pop_1~gender,
        notch = TRUE,
        ylim=c(0,100), 
        ylab="Estimated Size (in percentages)", cex.ylab = .3,
        main="Difference Between Male and Female Respondents'
        Estimates of Undocumented Hispanic Pop.", cex.main = 1.4,
        col=c("cyan3", "burlywood1"))
abline(h=16.7, lty = 9,
       lwd = 2,
       col='black')


t.test(hispanic_pop_1 ~ gender, data = mfs, mu=4.74)


## Problem 3 ##
trump_freq<-table(trump_approval)
trump_percent<-trump_freq/sum(trump_freq)*100
barplot(trump_percent, 
        main="Breakdown of Sample by Approval 
        for President Trump", 
        xlab="Level of Support for President Donald Trump
        (1=Strongly Approve, 5=Strongly Disapprove)", 
        ylab="Percent of Sample", 
        ylim=c(0,100), 
        col="darksalmon",
        cex.lab=.8,
        cex.axis=.7,
        cex.main=1.0)


## Problem 4 ##
summary(subset(mfs, pid == 1)$trump_approval, na.rm=TRUE)
summary(subset(mfs, pid == 2)$trump_approval, na.rm=TRUE)
summary(subset(mfs, pid == 3)$trump_approval, na.rm=TRUE)
summary(subset(mfs, pid == 4)$trump_approval, na.rm=TRUE)
IQR(subset(mfs, pid == 1)$trump_approval, na.rm=TRUE)
IQR(subset(mfs, pid == 2)$trump_approval, na.rm=TRUE)
IQR(subset(mfs, pid == 3)$trump_approval, na.rm=TRUE)
IQR(subset(mfs, pid == 4)$trump_approval, na.rm=TRUE)
sd(subset(mfs, pid == 1)$trump_approval, na.rm=TRUE)
sd(subset(mfs, pid == 2)$trump_approval, na.rm=TRUE)
sd(subset(mfs, pid == 3)$trump_approval, na.rm=TRUE)
sd(subset(mfs, pid == 4)$trump_approval, na.rm=TRUE)


plot(jitter(pid, factor=2), 
     jitter(trump_approval, factor=2),
     main="Trump Approval and Party Affiliation",
     col="darkseagreen2", pch=20,
     ylab="Approval For Pres. Trump",
     xlab="Party Affiliation")


t.test(trump_approval ~ pid_binary, data = mfs, mu=1.91)


## Problem 5 ##
set.seed(15251)
df<-data.frame(mfs)
newdata<-df[sample(nrow(df), 10), ]
mean(newdata$hispanic_pop_1, na.rm=TRUE)
sd(newdata$hispanic_pop_1, na.rm=TRUE)

set.seed(15251)
df<-data.frame(mfs)
newdata<-df[sample(nrow(df), 50), ]
mean(newdata$hispanic_pop_1, na.rm=TRUE)
sd(newdata$hispanic_pop_1, na.rm=TRUE)

set.seed(15251)
df<-data.frame(mfs)
newdata<-df[sample(nrow(df), 500), ]
mean(newdata$hispanic_pop_1, na.rm=TRUE)
sd(newdata$hispanic_pop_1, na.rm=TRUE)

set.seed(15251)
df<-data.frame(mfs)
newdata<-df[sample(nrow(df), 1000), ]
mean(newdata$hispanic_pop_1, na.rm=TRUE)
sd(newdata$hispanic_pop_1, na.rm=TRUE)

summary(hispanic_pop_1)