#Boston-Car Dataset

#1st Step: Data Aquisition

library(MASS)
data("Boston")

#2nd Step: Dividing Dataset

set.seed(2)
library(caTools)
split<-sample.split(Boston$medv,SplitRatio = 0.7)
split
trainig_data<-subset(Boston,split==TRUE)
testing_data<-subset(Boston,split==FALSE)  
View(trainig_data)
View(testing_data)


#3rd Step: Exploratory Analysis

##Univariate Analysis
library(lattice)
splom(~Boston[c(1:6,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)

##Multivariate Analysis
cr<-cor(Boston)
install.packages("corrplot")
library("corrplot")
corrplot(cr,type="lower")


###Variance Inflation Factor: Vif, It Measure increase in variance, 1 means no correlation
install.packages('car')
library(car)

model<-lm(medv~.,data = trainig_data)
vif(model)
summary(model)

#4th Step: Implement Model

model<-lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+ptratio+black+lstat,data=trainig_data)
model
summary(model)

#5th Step: Optimize Model

model<-lm(medv~crim+zn+chas+nox+rm+dis+ptratio+black+lstat,data=trainig_data)
summary(model)


#6th Step: Model Validation

predic<-predict(model,testing_data)
predic
plot(testing_data$medv,lty=1.8,col="green")
lines(predic,col="blue")

#7th Step: Prediction

##predic<-predict(model,sample_data)
##predic



