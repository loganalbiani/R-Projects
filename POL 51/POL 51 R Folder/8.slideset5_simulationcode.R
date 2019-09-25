#Creating a Population
set.seed(525156)
poplec <- rep(0:1, c(490000, 510000))

#Sampling 50 from the Population

samp50 <- sample(poplec, 50, replace=TRUE)

samp50

mean(samp50)

#Generate a 100000 samples of size 50 from the population
set.seed(6788)
resample <- c()

for(i in 1:10000) resample[i]=mean(sample(poplec, 50))

#Creating the mean of the sampling distribution 

sampledistmean<-mean(resample)

#A Histogram of the Means

hist(resample, br=25, xlim=c(.1,.9), 
     xlab="Sample Estimate of p", 
     ylab="Frequency", 
     main="Many Means from Many Samples", col="blue") 

abline(v = sampledistmean)

#Increasing sample sizes

#Creating a Population
set.seed(525156)
poplec <- rep(0:1, c(490000, 510000))

#Sampling 50 from the Population

samp50 <- sample(poplec, 50, replace=TRUE)

samp50

mean(samp50)

#Sampling 100 from the Population 


samp100 <- sample(poplec, 100, replace=TRUE)

samp100

mean(samp100)

#Sampling 500 from the Population 


samp500 <- sample(poplec, 500, replace=TRUE)

samp500

mean(samp500)

