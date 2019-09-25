library(foreign)
library(car)
setwd("~/Dropbox/POL 51 Revamped/R code")
midterm<-read.csv("ps5data.csv")

midterm<-na.omit(midterm)


attach(midterm)

scatterplot(x=candidate_position, y=candidate_priority, pch="", xlab="Candidate Postion", ylab="Candidate Emphasis", 
     main="Position and Emphasis in 2010 House Elections", ylim=c(1,4), xlim=c(0,7))
   text(candidate_position, candidate_priority, repub_candidate, cex=.65)
   #legend("bottomleft", c("Republican=1", "Democrat=0"),  bty="n", cex=.65)


with(midterm[midterm$repub_candidate ==0,],scatterplot(x=candidate_position, y=candidate_priority, pch=20, xlab="Candidate Postion", ylab="Candidate Emphasis", 
     main="Position and Emphasis: Democrat", ylim=c(1,4), xlim=c(0,4), col="blue"))
 

 with(midterm[midterm$repub_candidate ==1,],scatterplot(x=candidate_position, y=candidate_priority, pch=20, xlab="Candidate Postion", ylab="Candidate Emphasis", 
     main="Position and Emphasis: Republican", ylim=c(1,4), xlim=c(4,7), col="red"))



#Computing correlation coefficient by hand
#Step 1: compute z-scores

demdata<-subset(midterm, repub_candidate==0)



zpos<-scale(demdata$candidate_position, center=TRUE, scale=TRUE)
zpri<-scale(demdata$candidate_priority, center=TRUE, scale=TRUE)


#Step 2: Compute the product of the standard units:

product<-zpos*zpri

#Step 3: Take the average of the product (using n-1 in the denominator)

rpospri<-sum(product, na.rm=TRUE)/150

rpospri

#Or we could do it the easy way: 

corpospri<-cor(demdata$candidate_position, demdata$candidate_priority)

corpospri

regline<-lm(zpri~zpos)
plot(zpos, zpri, xlim=c(-3, 3), ylim=c(-3,3), ylab="z-priority",
xlab="z-position", main="Scatterplot of Standardized Scores", col="blue")
abline(h=mean(zpos))
abline(v=mean(zpri))
abline(regline, lty=2, col="blue")
text(-2, 2, "zx*zy is NEGATIVE")
text(-2, -3, "zx*zy is POSITIVE")
text(2, -3, "zx*zy is NEGATIVE")
text(2, 2, "zx*zy is POSITIVE")
text(-2,-1, "r = -.43")

regmod<-lm(demdata$candidate_priority~demdata$candidate_position)
plot(x=demdata$candidate_position, y=demdata$candidate_priority, ylab="Democrat Priority", xlab="Democrat Position", main="Position and Priority for Democrats", pch=20, col="blue")
abline(regmod, lty=2)
abline(v=2.44)
abline(h=mean(demdata$candidate_priority))