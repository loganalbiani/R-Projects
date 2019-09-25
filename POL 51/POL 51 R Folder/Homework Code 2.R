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
        cex = .3,
        ylab="Social Dominance Preferences", cex.ylab = .3,
        main="Difference Between Republican and Democrat Respondents'
        Preferences for Social Dominance", cex.main = .8,
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
        cex = .3,
        ylab="System Justification Preferences", cex.ylab = .3,
        main="Difference Between Republican and Democrat Respondents'
        Preferences for Social Dominance", cex.main = .8,
        col=c("deepskyblue1", "firebrick1"))







###QUESTION 3: Correlation Between Soc. Dom./Sys. Just. and Dems/Reps ##
scatterplot(system_justification ~ social_dominance,
            xlab = "Social Dominance Amongst Respondents",
            ylab = "System Justification Amongst Respondents",
            cex = .2)
cor(system_justification, social_dominance, use = "complete.obs")

##For Dems - Correlation and Scatterplot - RUN ONE LINE AT A TIME##
dem <- subset(dataclass, repdem == 0)
detach(dataclass)
attach(dem)
scatterplot(system_justification ~ social_dominance,
            xlab = "Social Dominance Amongst Dems",
            ylab = "System Justification Amongst Dems",
            cex = .2)
cor(system_justification, social_dominance, use = "complete.obs")
detach(dem)
attach(dataclass)

##For Reps - Correlation and Scatterplot - RUN ONE LINE AT A TIME##
rep <- subset(dataclass, repdem == 1)
detach(dataclass)
attach(rep)
scatterplot(system_justification ~ social_dominance,
            xlab = "Social Dominance Amongst Reps",
            ylab = "System Justification Amongst Reps",
            cex = .2)
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
regmod2<-lm(restriction~social_dominance + system_justification + sex)
summary(regmod2)




### PROBLEM 8: CHAIN MIGRATION V. FAMILY REUNIFICATION ###
#Terminology: Chain Migration#
summary(subset(dataclass, chain_family == 0)$family_policy,
        na.rm=TRUE)
#Terminology: Family Reunification#
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

# Correlation and Boxplot of Latino Preferences on Restrictionist Policy #
cor(R_latino, restriction, use = "complete.obs")

# T-Test for This #
t.test(restriction~R_latino, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")

# Boxplot of these two variables #
R_latino[R_latino==0] <- "Other Ethnicities"
R_latino[R_latino==1] <- "Latinos"
boxplot(restriction~R_latino,
        cex = .3,
        ylab="Restrictionist Preferences", cex.ylab = .3,
        main="Lower Latino Restrictionst
Policy Preferences", cex.main = .8,
        col=c("lightblue1", "indianred2"))

#Linear Regression #
regmod2<-lm(restriction~R_latino)
summary(regmod2)


## PART 2: Whites and System Justification ##
summary(subset(dataclass, R_onlywhite == 0)$restriction,
        na.rm=TRUE)
summary(subset(dataclass, R_onlywhite == 1)$restriction,
        na.rm=TRUE)

# Correlation and Boxplot of Latino Preferences on Restrictionist Policy #
cor(R_onlywhite, system_justification, use = "complete.obs")

# T-Test for This #
t.test(system_justification~R_onlywhite, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")

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
regmod2<-lm(restriction~R_latino)
summary(regmod2)


##PART 3: Estimated Deaths Between Whites and Other Ethnicities
summary(subset(dataclass, R_onlywhite == 0)$estimate_dead,
        na.rm=TRUE)
summary(subset(dataclass, R_onlywhite == 1)$estimate_dead,
        na.rm=TRUE)

# Correlation and Boxplot of Latino Preferences on Restrictionist Policy #
cor(R_onlywhite, estimate_dead, use = "complete.obs")

# T-Test for This #
t.test(estimate_dead~R_onlywhite, data=dataclass, mu=0, paired=FALSE,
       alternative="two.sided")

# Boxplot of these two variables #
R_onlywhite[R_onlywhite==0] <- "Other Ethnicities"
R_onlywhite[R_onlywhite==1] <- "White"
boxplot(estimate_dead~R_onlywhite,
        cex = .3,
        ylab="Estimated Deaths", cex.ylab = .3,
        main="Lower White Estimates
of Immigrant Deaths", cex.main = .8,
        col=c("khaki", "lightsalmon"))

#Linear Regression #
regmod2<-lm(restriction~R_latino)
summary(regmod2)



### FIN ###









