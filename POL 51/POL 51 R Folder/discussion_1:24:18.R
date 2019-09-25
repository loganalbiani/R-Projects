###PREAMBLE###

### What if you only wanted to remove one particular variable? ###
#--------CODE HERE--------#

### Libraries that you might want to note###

install.packages("foreign")
library(foreign)

###We will keep adding to this as we move on###

###Univariate Statistics###

###Let's begin with loading a data - mtcars from the R data repository###

mydata <-mtcars
attach(mydata)

###Computing some univaraite statistic How would you handle missing data here?###

mean(mpg, na.rm=TRUE)
sd(mpg, na.rm=TRUE)
median(mpg, na.rm=TRUE)

median(mydata$mpg)

mean(mydata$gear)

###Here's a quick way to get a summary:


plot(mydata$mpg)

hist(mydata$mpg)

plot(mpg)
plot(mpg, type = "l")
hist(mydata$mpg)
barplot(mpg)

###What's the difference between a bar graph and a histogram????###


###PICKING UP NEW DISCUSSION 2/7/18###

dotchart(mpg)

?dotchart()

dotchart(mtcars$mpg, main="MPG of Specific Cars", labels = row.names(mtcars),
         cex = .6, xlab = "mpg", groups = cyl)

plot(mpg, cyl, main="scatterplot Example",
     xlab="miles per gallon", ylab="weight of cars")

plot(mpg, wt, main="Scatterplot Example",
     xlab="Miles Per Gallon", ylab="Weight of the car",
     pch=19)