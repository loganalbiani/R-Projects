#This is an annotated R script using the epi data to illustrate correlation and regression.  
#This script requires packages foreign and car.   


library(foreign)
library(car) #Must install this to do the scatterplot w/regression overlay 

#Setting the working directory so I can find the data on my computer
setwd("~/Dropbox/POL 51 Revamped/R code")

#Reading in the data:

epidata<-read.dta("epimissRM.dta")

attach(epidata)

#Scatter plots

plot(loggdp, epi, xlim=c(-4, 3), ylim=c(30,95), ylab="EPI",
xlab="log(GDP)", main="Scatterplot of EPI by log(GDP)", col="blue")

#Scatterplot using the car packagae

scatterplot(epi ~ loggdp, xlim=c(-4, 3), ylim=c(30,95), ylab="EPI",
xlab="log(GDP)", main="Scatterplot of EPI by log(GDP)")

#Finding the point of averages and the standard deviations

meanepi<-mean(epi, na.rm=TRUE)
meangdp<-mean(loggdp, na.rm=TRUE)
sdepi<-sd(epi, na.rm=TRUE)
sdgdp<-sd(loggdp, na.rm=TRUE)

#Plotting the scatterplot with lines for +- 2 standard deviations on x and y

plot(loggdp, epi,  ylab="EPI",
xlab="log(GDP)", main="Scatterplot of EPI by log(GDP)", col="blue")
abline(h=meanepi)
abline(v=meangdp)
abline(h=meanepi + 2*sdepi)
abline(h=meanepi - 2*sdepi)
abline(v=meangdp + 2*sdgdp)
abline(v=meangdp - 2*sdgdp)

#Computing correlation coefficient by hand
#Step 1: compute z-scores

zepi<-scale(epi, center=TRUE, scale=TRUE)
zgdp<-scale(loggdp, center=TRUE, scale=TRUE)


#Step 2: Compute the product of the standard units:

product<-zepi*zgdp

#Step 3: Take the average of the product (using n-1 in the denominator)

repigdp<-sum(product, na.rm=TRUE)/161

repigdp

#Or we could do it the easy way: 

corepigdp<-cor(epi, loggdp)

corepigdp

#Plot the data w/zy*zx quadrants.

plot(zgdp, zepi, xlim=c(-3, 3), ylim=c(-3,3), ylab="zEPI",
xlab="zlog(GDP)", main="Scatterplot of zEPI by zlog(GDP)", col="blue")
abline(h=mean(zepi))
abline(v=mean(zgdp))
text(-2, 2, "zx*zy is NEGATIVE")
text(-2, -3, "zx*zy is POSITIVE")
text(2, -3, "zx*zy is NEGATIVE")
text(2, 2, "zx*zy is POSITIVE")

#Regression Analysis

par(mfrow=c(2,2))

#First consider the data: 

plot(sort(loggdp), ylab="log(GDP)", main="Index Plot of log(GDP)",
col="blue")

plot(sort(epi), ylab="EPI", main="Index Plot of EPI",
col="blue")

plot(loggdp, epi, xlim=c(-4, 3), ylim=c(0,100), ylab="EPI",
xlab="log(GDP)", main="Scatterplot of EPI by log(GDP)", col="blue")
abline(h=meanepi)
abline(v=meangdp)

par(mfrow=c(1,1))

#Regression Model

#Using the lm package to estimate a linear model: (lm stands for "linear model")

regmod<-lm(epi~loggdp)

#Retrieving the predicted values of EPI

yhat<-regmod$fitted.values

#Plotting the data with the fited regression line

plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Scatterplot of EPI by log(GDP) w/Regression Line", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")

#Plotting the data with the fited regression line
#along with the point of averages. 

plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Scatterplot of EPI by log(GDP) w/Regression Line", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")
abline(h=mean(epi))
abline(v=mean(loggdp))



#Computing the difference between y and yhat: the residual

residual<-yhat-mean(epi)

#Plotting data, the fitted values, and a reference line at the mean of EPI

plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Scatterplot of EPI by log(GDP)", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")
abline(h=mean(epi), col="red")

#Plotting data, the fitted values of 
#EPI along with residual lengths

plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Scatterplot of EPI by log(GDP) with Regression Line and Residuals", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")
arrows(loggdp, epi, loggdp, yhat, length=0, col="blue")


#Plotting data, the fitted values, and a reference line at the mean of
#EPI along with mean deviations

plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Scatterplot of EPI by log(GDP)", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")
abline(h=mean(epi), col="red")
arrows(loggdp, yhat, loggdp, mean(epi), length=0, col="orange")

#Computing the variance components for the regression model

#First, the sum of squares due to residual error

sserror <- sum((epi-yhat)^2)

#Second, the sum of squares "explained" by the model

ssmodel <- sum((yhat-mean(epi))^2)

#The R-squared is the ratio of the explained variance (ssmodel) to the
#total amount of variance.  Consider the two variance components: 

 par(mfrow=c(2,2))


plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Residuals ", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")
abline(h=mean(epi), col="red")
arrows(loggdp, epi, loggdp, yhat, length=0, col="blue")


plot(epi~loggdp, xlim=c(-4, 3), ylab="EPI", xlab="log(GDP)",
main="Explained", col="blue")
lines(yhat~loggdp, lty="twodash", col="red")
abline(h=mean(epi), col="red")
arrows(loggdp, yhat, loggdp, mean(epi), length=0, col="orange")



rsquare <- ssmodel/(ssmodel+sserror)

#Interpretation of the r-square?  About 37 percent of the total
#variation in the EPI scores is accounted for by the log(gdp) variable.

#Computing the root-mean-square error for the model: 

#First, let us compute the average of the squared residuals.  We 
#will call this the "mean square error": 

mse<-sserror/160 

#IF we take the square root of this, we get the average deviation
#around the regression line.  This is called the ROOT MEAN SQUARE ERROR

rmse<-sqrt(mse)


#Interpretation of the r.m.s.: it is the average deviation of points from
#the regression line.  If it is large, the points are, on average, far away
#from the fitted regression.  If it is small, the points lie close to the regression
#line.  Here, the average deviation is about +-9.87 points on the EPI scale. 

#Some intuition about the regression line.  The slope is the rate-of-change at
#which y changes with respect to a per-unit increase in x.  When x=0, the line
#intercepts the y-axis at the value of the intercept.  In the epi model, we found
#the regression to be: epi=61 + 5.6*loggdp  So IF log(gdp) were ever 0, the
#regression function would lead us to predict an EPI score of 5.6.  For each unit 
#increase in log(gdp), the predicted value for the EPI increases by about 5.6 points. 
#That is, as log(gdp) "runs" for one-unit, EPI rises by 5.6 units.  The intution of the slope
#is therefore easy:  slope=rise/run   Technically, a 1 standard deviation increase in log(gdp) is
#associated with r (the correlation coefficient) standard deviation increases in EPI.

#Since we know the correlation is .37 and we know the sd(epi) is 12.4 and we know the sd(log(gdp)) is 1.34, we have all the pieces to solve for the slope.  For a 1 standard deviation increase in log(gdp) (the run), EPI rises by about 5.6 units:  (r*sd(epi))/sd(loggdp)=5.6.  We can
#verify this easily: 

slope<- (corepigdp*sd(epi))/sd(loggdp)

#Note: the correlation of x,y is the same as the correlation of y,x.  This is NOT the case for regression.  Suppose we regressed loggdp on epi:

regmod2<-lm(loggdp ~epi)

yhat2<-regmod2$fitted.values

plot(loggdp~epi, xlab="EPI", ylab="log(GDP)",
main="Scatterplot of EPI by log(GDP) w/Regression Line", col="blue")
lines(yhat2~epi, lty="twodash", col="red")


#Standard Error of the Slope Coefficient

#Accounting for uncertainty in the estimate of beta_1 (slope coefficient)
#We know the estimate of the variance of the slope is: MSE/sum(x-xbar)^2
#We computed the MSE before.  The following will compute the denominator

varx<-sum((loggdp-mean(loggdp))^2)

#Variance of beta_1: 

varbeta<-mse/varx

#Standard Error of beta_1

sebeta<-sqrt(mse/varx)

#Now we have an estimate of the slope (5.64 for this model) and we
#have an estimate of the square root of the slope (.58).  If we think of
#the slope as our best estimate of the population slope, then we can test 
#a hypothesis

#H_0: beta_1=0

#The test-statistic is a t-test: 

tstat<-slope/sebeta

#We know the slope is 5.6 and the s.e. is .58.  The t-statistic is 9.73.  At
#any conventional level we would reject the null hypothesis. Technically, 
#we would check a t-table, but a t this large will easily be significant at any
#conventional level. 
