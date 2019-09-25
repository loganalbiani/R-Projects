

setwd("insert your wd here")


#I call the data object "mfs" but you can call it anything you want; however,
#you will need to replace "mfs" with your object name. 

mfs<-read.csv("pooled_data.csv", header=TRUE)

#Setting Gender codes 3 and 5 to missing data
mfs$gender[mfs$gender==3] <- NA 
mfs$gender[mfs$gender==5] <- NA 

##Currently the value of 1=Males and the value of 2=Females.  I want to turn 
#this into a binary variable such that 0=Male and 1=Female.  To do this,
#I must "recode" the gender variable. 
#Recoding gender variable to be 2=1 and 1=0

mfs$gender_binary[mfs$gender=="1"] <- 0
mfs$gender_binary[mfs$gender=="2"] <- 1

#Recoding Party ID because one student's survey had the incorrect coding
mfs$pid[mfs$pid==9] <- 1
mfs$pid[mfs$pid==10] <- 2
mfs$pid[mfs$pid==11] <- 3
mfs$pid[mfs$pid==12] <- 4

#Creating a dummy variable for partisan affiliation
#1=Republican
#0=Democrat

mfs$pid_binary[mfs$pid=="1"] <- 0
mfs$pid_binary[mfs$pid=="2"] <- 1



#Attaching the data
attach(mfs)

#Create code for questions 1-4 and 6



#For question 5, you can sample randomly from the full data set using this
#shell code.  You will need to do this multiple times to answer the question. Note to
#R purists, this is not the only way to do this so if you have a more creative way,
#feel free. 

#Resampling a sample of size 50
set.seed(15251)
df<-data.frame(mfs)
newdata<-df[sample(nrow(df), 50), ]

mean(newdata$hispanic_pop_1, na.rm=TRUE)
sd(newdata$hispanic_pop_1, na.rm=TRUE)

