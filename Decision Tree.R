### Data Engineering & Analysis ###
iris
str(iris)
summary(iris)
head(iris)

### Data Cleansing ###
# The ranges of values seem to be ok without any outliers
#There is equal distribution of the three classes -setosa,versicolor,virginia

### Explaratory Data Analysis ###
library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris,colour=Species,size=3) #High correlated
qplot(Sepal.Length,Sepal.Width,data=iris,colour=Species,size=3) #Problem detected!
par(mfrow=c(2,2))

boxplot(Petal.Length ~ Species, data=iris,col="red")
title("Petal Length")

boxplot(Petal.Width ~ Species , data=iris, col="green")
title("Petal Width")

boxplot(Sepal.Length ~ Species , data=iris, col="blue")
title("Sepal Length")

boxplot(Sepal.Width ~ Species , data=iris , col="maroon")
title("Sepal.Width")

#All 3 except Sepal.Width seem to bring the significan difference between the 3 classes.

#Correlations 
pairs.panels(iris)

### Modeling and Prediction ###

#Split training and test set in the ration of 70-30
library(caret)
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)
training<-iris[inTrain,]
testing<- iris[-inTrain,]
dim(training);dim(testing)

table(training$Species); table(testing$Species)

#Model Building 

library(C50)
model <- C5.0(training[-5],training$Species) 
# [-5], because we take out the target variable.
#the C5.0 DT algorith will be used
summary(model) # Seems like a good model with few errors

#Testing 
#Let's predict the class of each sample in the test data.
#Then compare the prediction with the actual value of the class
library(e1071)
predicted<-predict(model,testing)
table(predicted)
confusionMatrix(predicted,testing$Species)
# Great accuracy 

#Get only Sepal Length, width and species
sub_data<-iris[,c(1,2,5)]
summary(sub_data)
nTrain<-createDataPartition(y=sub_data$Species,p=0.7,list=FALSE)
training<-sub_data[inTrain,]
testing<- sub_data[-inTrain,]
dim(training);dim(testing)
model <- C5.0(training[-5],training$Species) 
summary(model)
#High error levels
predicted <- predict(model,testing)
confusionMatrix(predicted,testing$Species)
# Accuracy suffers !
#This shows the impact of the correlation
