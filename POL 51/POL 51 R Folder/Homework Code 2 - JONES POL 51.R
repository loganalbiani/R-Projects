## RUN LINES 3-8 PRIOR TO ENGAGING CODE FOR PROBLEMS ##

setwd("~/Desktop/POL 51 R Folder")
library(foreign)
dataclass<-read.dta("problemset2data.dta")
attach(dataclass)
install.packages("car")
library(car)





## QUESTION 1: Social dominance for Dems/Reps ##
## Summary stats for Dems - Run one line at a time ##
summary(subset(dataclass, pid_root == 2)$social_dominance,
        na.rm=TRUE)

## Summary stats for Reps ##
summary(subset(dataclass, pid_root == 1)$social_dominance,
        na.rm=TRUE)

## Conducting a t-test ##
attach(dataclass)

t.test(social_dominance~repdem,
       mu=0,
       alternative="two.sided")

## Boxplot for Visualization's Sake ##
pid_root[pid_root==1] <- "Republican"
pid_root[pid_root==2] <- "Democrat"
pid_root[pid_root==3] <- NA
pid_root[pid_root==4] <- NA
pid_root[pid_root==5] <- NA
pid_root[pid_root==6] <- NA
boxplot(social_dominance~pid_root,
        cex = .3, cex.lab=.7,cex.axis = .7,
        ylab="Social Dominance Orientation", cex.ylab = .3,
        main="Republicans Have Higher Social Dominance
Orientation Than Democrats", cex.main = .8,
        col=c("lightblue1", "indianred2"))






## QUESTION 2: System Justification for Dems/Reps ##
## Summary stats for Dems - Run one line at a time ##
summary(subset(dataclass, pid_root == 2)$system_justification,
        na.rm=TRUE)

## Summary stats for Reps - Run one line at a time ##
summary(subset(dataclass, pid_root == 1)$system_justification,
        na.rm=TRUE)

## Conducting a t-test ##
attach(dataclass)

t.test(system_justification~repdem,
       mu=0,
       alternative="two.sided")

## Boxplot for Visualization's Sake ##
pid_root[pid_root==1] <- "Republican"
pid_root[pid_root==2] <- "Democrat"
pid_root[pid_root==3] <- NA
pid_root[pid_root==4] <- NA
pid_root[pid_root==5] <- NA
pid_root[pid_root==6] <- NA
boxplot(system_justification~pid_root,
        cex = .3, cex.lab=.8,
        ylab="System Justification", cex.axis = .8,
        main="Republicans Have Higher System Justification
Preferences for System Justification", cex.main = .7,
        col=c("deepskyblue1", "firebrick1"))







###QUESTION 3: Correlation Between Soc. Dom./Sys. Just. and Dems/Reps ##
scatterplot(system_justification ~ social_dominance,
            xlab = "Social Dominance",
            ylab = "System Justification",
            cex = .2,
            main = "System Justification and Social Dominance 
Orientation Correlation", cex.main = .7, cex.lab=.8)
cor(system_justification, social_dominance, use = "complete.obs")

##For Dems - Correlation and Scatterplot - RUN ONE LINE AT A TIME WHEN ATTACHING/DETACHING DATA SETS##
dem <- subset(dataclass, repdem == 0)
detach(dataclass)
attach(dem)
scatterplot(system_justification ~ social_dominance,
            xlab = "Social Dominance",
            ylab = "System Justification",
            cex = .2, cex.main = .7, cex.lab = .8,
            main = "System Justification and Social Dominance
Correlation for Dems")
cor(system_justification, social_dominance, use = "complete.obs")
detach(dem)
attach(dataclass)

##For Reps - Correlation and Scatterplot - RUN ONE LINE AT A TIME WHEN ATTACHING/DETACHING DATA SETS##
rep <- subset(dataclass, repdem == 1)
detach(dataclass)
attach(rep)
scatterplot(system_justification ~ social_dominance,
            xlab = "Social Dominance",
            ylab = "System Justification",
            cex = .2, cex.main = .7, cex.lab = .8,
            main = "System Justification and Social Dominance
Correlation for Reps")
cor(system_justification, social_dominance, use = "complete.obs")
detach(rep)
attach(dataclass)






### QUESTION 4: Death/Crime Condition Effect on Restriction Policy ##
# Summary Statistics for Condition 0 (Crime) #
summary(subset(dataclass, death_crime == 0)$restriction,
        na.rm=TRUE)

# Summary Statistics for Condition 1 (Death) #
summary(subset(dataclass, death_crime == 1)$restriction,
        na.rm=TRUE)

# T-test for difference in Death v. Crime #
t.test(restriction~death_crime, mu=0, paired=FALSE,
       alternative="two.sided")






### QUESTION 6: AGGIE CHALLENGE ###
#Creating a Dummy Variable#
dataclass$ucd_TAMU[dataclass$TAMU== "1"] <- 0
dataclass$ucd_TAMU[dataclass$ucd== "1"] <- 1
dataclass$ucd_TAMU[dataclass$ucd_TAMU==0] <- "Fake (TAMU) Aggies"
dataclass$ucd_TAMU[dataclass$ucd_TAMU==1] <- "Real (UCD) Aggies"

#PART A: SUMMARY/T-TEST for UCD v. TAMU on Social Dominance#
summary(subset(dataclass, ucd==1)$social_dominance, na.rm=TRUE)
summary(subset(dataclass, TAMU==1)$social_dominance, na.rm=TRUE)

t.test(social_dominance~ucd_TAMU, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")


#PART B: SUMMARY/T-TEST for UCD v. TAMU on System Justification#
summary(subset(dataclass, ucd==1)$system_justification, na.rm=TRUE)
summary(subset(dataclass, TAMU==1)$system_justification, na.rm=TRUE)

t.test(system_justification~ucd_TAMU, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")

#PART C: SUMMARY/T-TEST for UCD v. TAMU in Respect to PID#
summary(subset(dataclass, ucd==1)$ideo9, na.rm=TRUE)
summary(subset(dataclass, TAMU==1)$ideo9, na.rm=TRUE)

t.test(ideo9~ucd_TAMU, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")

#PART D: SUMMARY/T-TEST for UCD v. TAMU in Resitriction#
summary(subset(dataclass, ucd==1)$restriction, na.rm=TRUE)
summary(subset(dataclass, TAMU==1)$restriction, na.rm=TRUE)

t.test(restriction~ucd_TAMU, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")

#PART E: SUMMARY/T-TEST for UCD v. TAMU in Undoc. Size Estimate#
summary(subset(dataclass, ucd==1)$undocumented_size, na.rm=TRUE)
summary(subset(dataclass, TAMU==1)$undocumented_size, na.rm=TRUE)

t.test(undocumented_size~ucd_TAMU, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")




### PROBLEM 7: Linear Regression ###
regmod1<-lm(restriction~social_dominance + system_justification + sex)
summary(regmod1)




### PROBLEM 8: CHAIN MIGRATION V. FAMILY REUNIFICATION ###
#Terminology: Family Reunification#
summary(subset(dataclass, chain_family == 0)$family_policy,
        na.rm=TRUE)
#Terminology: Chain Migration#
summary(subset(dataclass, chain_family ==1)$family_policy,
        na.rm=TRUE)

#T-Test of the Two#
t.test(family_policy~chain_family, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")




### PROBLEM 9: WILDCARD ###
##PART 1: Latino Preferences on Restrictionist Policy ##
# Summary of Latinos/Others Restriction Policy Preferences#
summary(subset(dataclass, R_latino == 0)$restriction,
        na.rm=TRUE)
summary(subset(dataclass, R_latino == 1)$restriction,
        na.rm=TRUE)

# T-Test for This #
t.test(restriction~R_latino, data=dataclass, mu=0, paired=FALSE,
       alternative="less")

# Boxplot of these two variables #
R_latino[R_latino==0] <- "Other Ethnicities"
R_latino[R_latino==1] <- "Latinos"
boxplot(restriction~R_latino,
        cex = .3,
        ylab="Restrictionist Preferences", cex.ylab = .2,
        main="Lower Latino Restrictionst
Policy Preferences", cex.main = .9,
        col=c("darkolivegreen1", "floralwhite"))

#Linear Regression #
regmod2<-lm(restriction~R_latino)
summary(regmod2)


## PART 2: Whites and System Justification ##
summary(subset(dataclass, R_onlywhite == 0)$system_justification,
        na.rm=TRUE)
summary(subset(dataclass, R_onlywhite == 1)$system_justification,
        na.rm=TRUE)

# T-Test for This #
t.test(system_justification~R_onlywhite, data=dataclass, mu=0, paired=FALSE,
       alternative="greater")

# Boxplot of these two variables #
R_onlywhite[R_onlywhite==0] <- "Other Ethnicities"
R_onlywhite[R_onlywhite==1] <- "White"
boxplot(system_justification~R_onlywhite,
        cex = .3,
        ylab="System Justification Preferences", cex.ylab = .1,
        main="Higher White System
Justification Preferences", cex.main = .8,
        col=c("limegreen", "papayawhip"))

#Linear Regression #
regmod3<-lm(system_justification~R_onlywhite)
summary(regmod3)




##PART 3: Correlation Between System Justification and Social Dominance ##
summary(restriction)
summary(system_justification)

# Correlation and Boxplot of Latino Preferences on Restrictionist Policy #
cor(system_justification, restriction, use = "complete.obs")

#Scatterplot of System Justification and Restriction
scatterplot(system_justification ~ restriction,
            xlab = "Restriction",
            ylab = "System Justification",
            cex = .2, cex.main = .9, cex.lab = .8,
            main = "Correlation Between System Justification
and Restriction Preferences")

### FIN ###









