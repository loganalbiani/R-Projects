# R code used in Week 1 
# R file 1.1
#B. Jones running/weight data for 2013-2017

#distance is an oject storing the total mileage run in the calendar month
#weight is an object storing the average monthly weight, in lbs. 
#time is an object storing a counter going from 1 (denoting Jan. 2013) to 60 (denoting Dec. 2017)

#Below I'm creating the object that I call "distance" by manually entering my data.  The "c" stands 
#"combine."  In many instances, you will read into R an external data set, like an Excel spreadsheet.  We 
#will learn to do this as the quarter goes on. 

distance<-c(76, 76, 162, 143, 233, 116, 124, 60, 35, 130, 249, 104, 
                     84, 115, 163, 180, 266, 133, 100, 230, 106, 63, 4, 0,
                     0, 0, 0, 1, 17, 0, 6, 0, 54, 14, 27, 27, 
                     66, 23, 16, 3, 48, 2, 6, 4, 0, 21, 2, 8,
                     34, 3, 12, 17, 12, 83, 102, 101, 85, 84, 81, 29)


weight<-c(185, 184, 180, 179, 175, 172, 172, 173, 172, 172, 172, 172, 
172, 171, 170, 165, 165, 165, 164, 164, 164, 164, 164, 165, 
170, 175, 177, 180, 182, 185, 185, 185, 185, 190, 190, 190,
188, 190, 191, 190, 191, 192, 191, 191, 192, 193, 194, 195,
195, 194, 196, 198, 197, 193, 187, 186, 184, 182,  183, 184 )


time<-(1:60)

#I can use "cbind" (which stands for column bind) to create a data set containing all the objects:

mydata<-cbind(distance, weight, time)

#If I just type "mydata" I can see what the data set looks like: 

mydata

#The "par" command is a graphical parameter command.  "par" is short for parameter.  Essentially,
#graphical parameters allow you to customize how your plots will look.  

#This command will allow me to do a "multi-panel" plot.  For now, let's supress the command.
#par(mfrow=c(2,2))


#Visual displays of data are a good first place to start.  Let's look at my 
#running data as "a function" of time.  

#I'm going to use an R function called plot.  If you want to see the help guide for this function,
#just type: ?plot

#R's generic plot function is actually quite powerful.  Let's quickly work through this: 

plot(distance~time, type="o", xlab="Months (1=Jan. 2013, 60=Dec. 2017)", ylab="Monthly Distance in Miles", 
     main="Monthly Distance, 2013-2017", pch=20, lty=1, col="skyblue1", cex.lab=.65, ylim=c(0,300))

#Below, I add some "decorations to the plot:
abline(v=22, lty=2, col="grey")
text(12, 3, "P.F injury (Oct. 2014)==>", cex=.5)
text(55, 110, "Last Half of 2017", cex=.5)

plot(weight~time, type="o", pch=20, lty=1, col="orange", cex.lab=.65, xlab="Months (1=Jan. 2013, 60=Dec. 2017)", ylab="Average Monthly Weight in Pounds", main="Monthly Weight, 2013-2017", ylim=c(150,200))
abline(v=22, lty=2, col="grey")
text(12, 153, "P.F injury (Oct. 2014)==>", cex=.5)

plot(weight~distance, pch=20, xlab="Distance in Miles", ylab="Weight in Pounds", col="blue", cex.lab=.65, main="Distance and Weight")
abline(lm(weight~distance), lty=2, col="grey")