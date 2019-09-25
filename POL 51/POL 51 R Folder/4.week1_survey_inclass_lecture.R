
#This is a comment
#extend 

#STEP 1: Working Directories

#Setting the working directory. Here is the quick way to do it (go to Session)

setwd("~/Dropbox/POL 51 Revamped/R code")

#setwd("/Users/bradfordjones/Dropbox/POL 51 Revamped/R code/")

#Step 2: Reading in an external data set.

#What are libraries? What are packages? 

library(foreign)

#Objects

data_class<-read.csv("week1survey.csv")

#Attaching the object

attach(data_class)

#Summarizing the object 

summary(data_class)

#Step 3: Analysis...what are you going to do with the data? 

#Start with univariate/descriptive statistics 

#Average/Mean:  x-bar=sum(x)/n  

mean(wall_slider_1, na.rm=TRUE)

#Nonresistant statistic so it's affected by SKEW

#Plot (Go through code): 

hist(wall_slider_1, 
     xlim=c(0,100),
     ylab="Frequency", 
     xlab="Level-of-support for border wall",
     col="red", 
     main="Respondents are strongly opposed to border wall", 
     breaks=20)

#R TRICK

#Suppose we want to plot the percentages instead of frequencies? 

#Turning the frequencies into percentages
wall_table<-table(wall_slider_1)
wall_percent<-wall_table/sum(wall_table)*100

#Now let's plot the percentages and use the barplot cmd: 

barplot(wall_percent, 
        main="Most respondents strongly oppose border wall", 
        xlab="Level of Support for Wall", 
        ylab="Percent of Sample", 
        ylim=c(0,50), 
        col="red")


#Plot interpretation: what do we see?  Data 
#exhibit SKEWNESS. 

#Two kinds of skew: RIGHT and LEFT

#The mean is not resistant

dv<-c(10, 20, 30, 40, 50)
mean(dv)
dv2<-c(10, 20, 30, 40, 500000)
mean(dv2)

#The mean is pulled toward the outlying observation.  
#Think income and Jeff Bezos

#With heavy skew, the mean is not always a good
#descriptor of central tendency

#Another statistic: THE MEDIAN 

median(wall_slider_1, na.rm=TRUE)

#Median score is 2; Mean score is 16.3

#Median < Mean.  When this happens the data 
#are RIGHT SKEWED.   When Median>Mean, data are
#LEFT SKEWED.  (Why right and left?)

#Computing median by hand?  It's a locational measure: 

median(dv)

#For odd data: (n+1)/2  6/2=3
#The third data point in the ordered data is the median:

dv

#For even data, the median is the average of the two
#central points: 

dv3<-c(10, 20, 30, 40, 50, 60)
median(dv3)

#Median is a RESISTANT Statistic: 

#Remember dv2? 

mean(dv2)

median(dv2)

#Pro-tip: report both in quantitative research.  

#Mean and median are measures of central tendency.  But we 
#often want to know about DISPERSION or variation. 

#GPA and low dispersion: 3.8 with an average deviation 
#of .1 means that the mean is high and the variation is
#low.  On average, everyone is smart. 

#Dispersion around the mean is called the 
#STANDARD DEVIATION 

#Let's change to a difference variable: Ideological self-placement.

#Let's analyze ideological placement: 0=Extreme Conservative; 100=Extreme Liberal.

hist(ideo_slider_1, 
     xlim=c(0,100),
     ylab="Frequency", 
     xlab="Ideological self-placement",
     col="red", 
     main="Sample indicates high-level of liberal self-placement", 
     breaks=20)

#Let's compute mean and median: 

mean(ideo_slider_1, na.rm=TRUE)
median(ideo_slider_1, na.rm=TRUE)


#Are these data skewed?  What does the plot reveal? 

#Mean is at about 68.3 but there is a lot of VARIATION
#around the average.  The question is: what is 
#the AVERAGE amout of variation?  

#This would be the STANDARD DEVIATION

sd(ideo_slider_1, na.rm=TRUE)

#"Words: For this sample, average ideologyical self-placement
#is at 68.3 indicating students are on average, ideologically
#more liberal than conservative.  The average deviation from
#this mean is about 18.6 points."

#How big? How small?  That's a relative question.  What if
#the standard deviation was 0?  

#STATISTICS ALERT: Connection of mean and standard deviation
#to inference and the normal distribution 

#####Back to dispersion around the mean.  The 
#standard deviation is a function of the variance.  The
#variance is the average of the SQUARED DEVIATIONS from the
#mean.  Who thinks in terms of squared deviations? 

#NO ONE! 

#For ideology: 

var(ideo_slider_1, na.rm=TRUE)

#Average SQUARED deviation for the ideology
#scale is 347.5.  Makes no natural sense to
#report this.  

#But if something is in squared terms, JUST TAKE THE 
#SQUARE ROOT.  sqrt(var)=sd

sqrt(var(ideo_slider_1, na.rm=TRUE))

#Which is?  

sd(ideo_slider_1, na.rm=TRUE)

#So that's our measure of dispersion around the mean? 

#What about the median?   

#We will use QUARTILES. 

#The median is the center part of the data.  It's the 
#50th PERCENTILE.  It splits the data in half.  

#So IMAGINE THE AREA BELOW THE MEDIAN.  If you split that area
#in half, you would have the 25th PERCENTILE (or the First Quartile)

#IMAGINE THE AREA ABOVE THE MEDIAN.  IF you split
#that in half, you'd have the 75th PERCENTILE (or the 
#THIRD QUARTILE)

#The "distance between the 3rd and 1st quartile is called
#the INTERQUARTILE RANGE OR IQR

IQR(ideo_slider_1)

#This is a measure of deviation around the 
#median.

#Here is a quick way to get a summary of a variable:
summary(ideo_slider_1)

#Throw out the mean and you have 5 numbers left.  
#This is "Tukey Five Number Summary

#This gives Tukey Five-Number summary: (Note it's redundant w/the summary cmd.)
fivenum(ideo_slider_1)

#Visualizing the data: BOX PLOTS

#Having fun with box plots
  boxplot(ideo_slider_1,  
          ylab=c("Ideological Self-Placement"), 
          ylim=c(0,100), 
          main="Boxplot of ideological self-placement", 
          col="blue")

  #Giving Labels for Gender variable
  gender[gender==1] <- "Male"
  gender[gender==2] <- "Female"
  
#Examining relationship between gender and ideology.  First
  #let's subsect data and get descriptive statistics: 
mean(subset(data_class, gender ==1)$ideo_slider_1, na.rm=TRUE)
mean(subset(data_class, gender ==2)$ideo_slider_1, na.rm=TRUE)
sd(subset(data_class, gender ==1)$ideo_slider_1, na.rm=TRUE)
sd(subset(data_class, gender ==2)$ideo_slider_1, na.rm=TRUE)
median(subset(data_class, gender ==1)$ideo_slider_1, na.rm=TRUE)
median(subset(data_class, gender ==2)$ideo_slider_1, na.rm=TRUE)

#Box plot of ideology by gender

boxplot(ideo_slider_1~gender, 
        ylim=c(0,100), 
        ylab=c("Ideological Self-Placement"), 
        main="Female respondents have higher \ndegree of liberal self-placement",
        col=c("blue", "orange"))

#Difference-in-means of ideology by gender: 

t.test(ideo_slider_1~gender)

