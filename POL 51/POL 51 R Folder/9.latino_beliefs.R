
library(foreign)


setwd("~/Dropbox/Immigration Death Study 2018/analysisfiles")

  

data<-read.dta("undoc_estimate.dta")

attach(data)

par(mfrow=c(2,2))
#Dotchart: Sorted and made visually more appealing
dotchart(sort(slider_undoc),cex=.7,
         xlab="Estimated Size (in percentages)",
         pch=20, col="black",
         main="Individuals overestimate size of \nundocumented Hispanic population",
         lcolor="transparent")
abline(v=16.7, lty=2, col="black")

dotchart(sort(nonlat_estimate),cex=.7,
         xlab="Estimated Size (in percentages)",
         pch=20, col="skyblue2",
         main="Nonlatinos overestimate size of \nundocumented Hispanic population",
         lcolor="transparent")
abline(v=16.7, lty=2, col="black")


dotchart(sort(lat_estimate),cex=.7,
         xlab="Estimated Size (in percentages)",
         pch=20, col="orange",
         main="Latinos overestimate size of \nundocumented Hispanic population",
         lcolor="transparent")
abline(v=16.7, lty=2, col="black")

#Side-by-Side Boxplot by Gender

#Giving Labels for Gender variable
R_latino[R_latino==1] <- "Latino"
R_latino[R_latino==0] <- "Non-Latino"


boxplot(slider_undoc~R_latino, col=c("orange","skyblue2"),
        ylab="Percentage of Sample", main="Boxplots of Unodcumented Estimates")
abline(h=16.7, lty=2, col="black")

summary(nonlat_estimate)
summary(lat_estimate)

#t-test 1: Do non-Latina/os significantly overstate the size of the 
#undocumented Hispanic population? 

#If no, then mean estimate should be more-or-less equal to "known" 
#estimate of about 17 percent. Let's test it.  

#What is the mean and s.d.?

mean(nonlat_estimate, na.rm=TRUE)
sd(nonlat_estimate, na.rm=TRUE)

#From the data set, we know that 732 non-Latinos answered this question. 
#This means for a 1 group t-test, we have 731 degrees-of-freedom


#From lecture slides and from p. 240 of (my version) book: 

#t=(x-bar - mu)/s/sqrt(N)

#Compute the t-statistic "by hand" (note there will be rounding differences)

t_byhand<-(23.21-17)/(18.68/27.06)
t_byhand

#The "t-test" asks: what's the probability of getting a number this large or larger? (In A.V.)?

#Use the "pt" function in R.  pt by defaul gives area to the left of t (lower tail): 

pt(8.99, 731)
pt(8.99, 731, lower.tail=FALSE)

#One-group t-test formula:  

t.test(nonlat_estimate,
       mu=17, alternative="greater")

#Suppose condition of the null were 23?" 

t_byhand2<-(23.21-23)/(18.68/27.06)
t_byhand2

#The "t-test" asks: what's the probability of getting a number this large or larger? (In A.V.)?

#Use the "pt" function in R.  pt by defaul gives area to the left of t (lower tail): 

pt(.30, 731)
pt(.30, 731, lower.tail=FALSE)

#Let's do the test

t.test(nonlat_estimate,
       mu=23, alternative="greater")

#So what do we know? We know non-Latinos signficantly overreport
#the size of the undocumented Latina/o population.  

#OK...what about Latina/o respondents? 

mean(lat_estimate, na.rm=TRUE)
sd(lat_estimate, na.rm=TRUE)

#We know 255 Latinx gave answers to this question. 

t_byhand3<-(26.47-17)/(18.91/sqrt(255))
t_byhand3

pt(7.99, 254, lower.tail=FALSE)


t.test(lat_estimate,
       mu=17, alternative="greater")

#So what do we know? We know Latinos signficantly overreport
#the size of the undocumented Latina/o population.  

#Ok so what about the difference between Latinos and non-Latinos? 

#Two-group t-test: 

#t=(Mean_1 - Mean_2)/sqrt(var_1/n_1 + var_2/n_2)

#We need six pieces of information: 

#Mean of group 1
#Mean of group 2
#Variance of group 1
#Variance of group 2
#N for group 1
#N for group 2 

mean(nonlat_estimate, na.rm=TRUE)
mean(lat_estimate, na.rm=TRUE)
var(nonlat_estimate, na.rm=TRUE)
var(lat_estimate, na.rm=TRUE)

#Null Hypothesis: 

#We already know Latina/o ratings > Nonlatina/o ratings in the data.  

#Research question is: is this difference signficiantly different from 0? 

#H_0: Mean_L-Mean_NL=0 

#Alternative Hypothesis? 

#H_A: Mean_L > Nean_NL 

#Two-group t "by hand" using the 6 pieces of information: 

twogroup_byhand=(26.47-23.21)/sqrt((357.74/255) + (349.04/732))
twogroup_byhand

#What the probability of our test-statistic? 

#What are the degrees-of-freedom here? 

#1-tail probability: 
pt(2.38, 985)

#In words, this is the probability area under the t to the left of 2.38.

#The t distribution is symmetrical so: 

pt(2.38, 985, lower.tail=FALSE)

#gives us the area ABOVE t=-2.38. 

#Note: 

pt(-2.38, 985) + pt(-2.38, 985, lower.tail=FALSE)

#which means if we sum the lower and upper tail area, it will have to summ to 1.0.  

#2-tail probability: 
2*pt(2.38, 985, lower.tail=FALSE)

#Probability area and the t and the z.  Let's do some high-tech teaching! 

#(Go to z and t tables)

#We don't need to do any of this "by hand" but it helps if we understand it.  Let's 
#make R do the work. 

#Exact test (assuming equal variance)

t.test(slider_undoc~R_latino, 
       var.equal=TRUE, alternative="greater")

#Conclusion: there is a significant difference between non-Latino and Latinos in terms
#of their population estimates such than L estimates are signficantly higher than Latino estimates.

#Two-sided alternative?  

t.test(slider_undoc~R_latino, 
       var.equal=TRUE, alternative="two.sided")


#Some more fun: difference between Latino estimates and Latino beliefs about other's estimates. 

par(mfrow=c(1,1))

boxplot(slider_undoc, slider_meta_undoc, 
        ylim=c(0,100), 
        ylab="Estimated Size (in percentages)",
        main="Latina/o estimates of and metabeliefs about \nLatina/o undocumented population size",
        col=c("skyblue2", "orange"),
        names = c("Estimate", "Metabelief"))
abline(h=16.7, lty=2, col="black")


#We could create a difference measures and then do a 1-group t-test:

ediff<-slider_undoc - slider_meta_undoc

mean(ediff, na.rm=TRUE)
sd(ediff, na.rm=TRUE)

dotchart(sort(ediff),cex=.7,
         xlab="Estimated Size (in percentages)",
         pch=20, col="red",
         main="Latina/os believe non-Latinos overstate size of \nundocumented Hispanic population",
         lcolor="transparent")
abline(v=0, lty=2, col="black")

#One group t-test against zero (why 0???)

#By Hand

mean(ediff, na.rm=TRUE)
sd(ediff, na.rm=TRUE)

t_byhand4<-(-14.75-0)/(23.80/sqrt(255))
t_byhand4

pt(-9.90, 254)

t.test(ediff, mu=0, alternative="less")

#t-test against -10

t.test(ediff, mu=-10, alternative="less")



