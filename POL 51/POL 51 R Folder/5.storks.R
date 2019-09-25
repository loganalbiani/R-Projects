   #R Script: Babies and Storks
   #Data are from: "Storks Deliver Babies (p=0.008)"
   # Robert Matthews. 2000. _Teaching Statistics_. 22: 36-38.
        
    
#Vectors of Data    
    
    storks<-c(100, 300, 1, 5000, 9, 140, 3300, 2500, 4, 5000, 5,
      30000, 1500, 5000, 8000, 150, 25000)
    
    birth<-c(83, 87, 118, 117, 59, 774, 901, 106, 188, 124, 
    551, 610, 120, 367, 439, 82, 1577)
    
    landarea<-c(28750, 83860, 30520, 111000, 43100, 544000, 357000, 132000, 41900, 93000, 301280, 312680, 92390, 237500, 
     504750, 41290, 779450)
    
    data<-data.frame(cbind(storks, birth, landarea))

#Correlation
corcoef<- cor(birth, storks)
corcoef

#Regression Model
linearmodel<- lm(birth ~ storks)
summary(linearmodel)

       plot(birth ~ storks, xlab="Stork Breeding Pairs", ylab="Birth Rates", main="Birth Rates and Storks")
       lines(linearmodel$fitted ~ storks, type="o", lty="solid" , col="red")
       
       
   #Correlation
corcoef2<- cor(storks, landarea)
corcoef2

#Regression Model
linearmodel2<- lm(storks ~ landarea)
summary(linearmodel2)

       plot(storks ~ landarea, xlab="Land Area (km^2)", ylab="Stork Breeding Pairs", main="Storks and Land Area")
       lines(linearmodel2$fitted ~ landarea, type="o", lty="solid" , col="red")
    
