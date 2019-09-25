#My New Survey

setwd("~/Dropbox/POL 51 Revamped/R code")

mfs<-read.csv("POL_051__Survey.csv", header=TRUE)

attach(mfs)

#Look at the gender variable: 

table(gender)

#Currently the value of 1=Males and the value of 2=Females.  I want to turn 
#this into a binary variable such that 0=Male and 1=Female.  To do this,
#I must "recode" the gender variable.  This is simple to do in this case.  If
#I just subtract the current value of gender by 1, this creates a binary variable:

#Recoding gender variable to be 2=1 and 1=0

gender_binary<-gender-1

#Now look at the table: 

table(gender_binary)

#Let's look at the ratings for the size of the undocumented population  
#Specifically the question asked: 
#We hear a lot of talk these days about undocumented immigration. 
#Using the sliding scale, give your best guess of the percentage 
#of the Hispanic population living in the U.S. that is undocumented. 
#For example, if you indicated "1", you would be saying that 
#1 percent of the Hispanic population is undocumented.

#Summary Statistics: 

summary(hispanic_pop_1)

#Let's do a bar plot of the percentages

#Turning the frequencies into percentages
hispanic_table<-table(hispanic_pop_1)

#Now creating percentages 
hispanic_percent<-hispanic_table/sum(hispanic_table)*100

#Now let's plot the percentages and use the barplot cmd: 

barplot(hispanic_pop_1, 
        main="Most respondents overstate size of undocumented pop.", 
        xlab="Estimate of Size", 
        ylab="Percent of Sample", 
        ylim=c(0,100), 
        col="red")

# Dotchart: Unsorted 
dotchart(hispanic_pop_1, cex=.7,
        xlab="Estimated Size (in percentages)",
        main="People overestimate size of \nundocumented Hispanic population")
        abline(v=16.7)

#Dotchart: Sorted
dotchart(sort(hispanic_pop_1),cex=.7,
         xlab="Estimated Size (in percentages)",
         main="People overestimate size of \nundocumented Hispanic population")
abline(v=16.7)

#Dotchart: Sorted and made visually more appealing
dotchart(sort(hispanic_pop_1),cex=.7,
         xlab="Estimated Size (in percentages)",
         pch=20, col="red",
         main="People overestimate size of \nundocumented Hispanic population")
abline(v=16.7, lty=2, col="gray")

#Let's look at Trump approval question: 

#Do you approve or disapprove of the way President Trump is handling his job as President?
#Strongly Approve  (1) 
#Moderately Approve  (2) 
#Neither Approve nor disapprove  (3) 
#Moderately Disapprove  (4) 
#Strongly Disapprove  (5) 

#Creating a table for frequencies

trump_freq<-table(trump_approval)

#Turning the frequencies into percentages

trump_percent<-trump_freq/sum(trump_freq)*100

#Let's build up a bar-plot

#Basic Plot
barplot(trump_approval)

#Plot with fancy stuff
barplot(trump_approval, 
        main="Most respondents strongly disapprove of Trump", 
        xlab="Level of Support for Trump (5=Strongly Disapprove)", 
        ylab="Percent of Sample", 
        ylim=c(0,100), 
        col="cadetblue",
        cex.lab=.7,
        cex.axis=.7,
        cex.main=.9)

mean(hispanic_pop_1, na.rm=TRUE)
