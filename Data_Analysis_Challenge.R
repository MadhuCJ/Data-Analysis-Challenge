#Data_Analysis_Challenge_Data_Set
#Data is read
library(readr)
Data_Analysis_Challenge_Data_Set <- read_csv("C:/Users/Prakash/Desktop/Data Analysis Challenge Data Set.csv")
 View(Data_Analysis_Challenge_Data_Set)
#attached the file to use the data present in the file
 attach(Data_Analysis_Challenge_Data_Set)
#Statistical data
summary(`wheel-base`)
 summary(length)
 summary(width)
 summary(height)
 summary(`curb-weight`)
 summary(`engine-size`)
 summary(bore)
 summary(stroke)
 summary(`compression-ratio`)
 summary(horsepower)
 summary(`peak-rpm`)
 summary(`city-mpg`)
 summary(`highway-mpg`)
#Standard Deviation
 sd(`wheel-base`)
 sd(length)
 sd(width)
 sd(height)
 sd(`curb-weight`)
 sd(`engine-size`)
 sd(`compression-ratio`)
 sd(`city-mpg`)
 sd(`highway-mpg`)
#Two dimensional Analysis
 hist(Data_Analysis_Challenge_Data_Set$`wheel-base`)
 hist(Data_Analysis_Challenge_Data_Set$length)
 hist(Data_Analysis_Challenge_Data_Set$width)
 hist(Data_Analysis_Challenge_Data_Set$height)
 hist(Data_Analysis_Challenge_Data_Set$`curb-weight`)
 hist(Data_Analysis_Challenge_Data_Set$`engine-size`)
 hist(Data_Analysis_Challenge_Data_Set$`compression-ratio`)
 hist(Data_Analysis_Challenge_Data_Set$`city-mpg`)
 hist(Data_Analysis_Challenge_Data_Set$`highway-mpg`)
 hist(Data_Analysis_Challenge_Data_Set$`compression-ratio`)
#Panel data Analysis
 library(plm)
 mydata<-read.csv("C:/Users/Prakash/Desktop/Data Analysis Challenge Data Set.csv")
 attach(mydata)
#X parameters
 X<-cbind(length,width,height)
#Creation of panel data
 pdata<-plm.data(mydata, index=c("symboling"))
#Y parameters
 Y<- cbind(`curb-weight`)
#Summary of the dependant and independant variables.
 summary(Y)
 summary(X)
#Pooling Method
 pooling<-plm(Y~X,data=pdata,model="pooling")
 summary(pooling)
#Between Estimator
 between<-plm(Y~X,data=pdata,model="between")
 summary(between)
#First Difference Estimator
 FirstDifference<-plm(Y~X,data=pdata,model="fd")
 summary(FirstDifference)
#Within Estimator
 WithinEstimator<-plm(Y~X,data=pdata,model="within")
 summary(WithinEstimator)
#Random Effects Estimator
 RandomEffects<-plm(Y~X,data=pdata,model="random")
 summary(RandomEffects)
#LM Test
 plmtest(pooling)
 pFtest(FirstDifference,pooling)
 phtest(RandomEffects,FirstDifference)

 
