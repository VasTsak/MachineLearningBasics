#The input data contains surveyed information about potential customers to a bank.
#The goal is to build a model that would predict if prospect would become  a customer
#bank, if contacted by a marketing exercise.

#Techniques used
#Random Forest
#Training and Testing
#Confusion Matrix
#Indicator Variables
#Binning
#Variable reduction

## Data Engineering & Analysis
bank_data<-read.table("bank.csv",header = TRUE,sep=";")
str(bank_data)
summary(bank_data)

##Data Cleansing
library(psych)
#Given the large number of variables, we will split the analysis into two sets.
pairs.panels(bank_data[,c(1:8,17)])
pairs.panels(bank_data[,9:17])

#Based on the correlation co-efficients, let us eliminate default ,
#balance,day,month,campaign,poutcome because of very low correlation.
#There are others too with very low correlation, but let us keep it 
#for example sake.

new_data <-bank_data[,c(1:4,7:9,12,14,15,17)]
str(new_data)
pairs.panels(new_data)

## Data Transformations
#Convert age into a binned range
#Convert marital status into indicator variable.It can be done with 
#the other factors but we choose not to do it.
new_data$age<-cut(new_data$age,c(1,20,40,60,100))
new_data$is_divorced<-ifelse(new_data$marital=="divorced",1,0)
new_data$is_single<-ifelse(new_data$marital=="single",1,0)
new_data$is_married<-ifelse(new_data$marital=="married",1,0)
new_data$marital<-NULL
str(new_data)

##Exploratory Analysis
par(mfrow=c(2,2),las=2)
plot(new_data$housing,new_data$y,
     xlab="Housing",ylab="Become Customer?",col=c("darkgreen","red"))
plot(new_data$contact,new_data$y,
     xlab="contact",ylab="Become Customer?",col=c("darkgreen","red"))
boxplot(duration~y,data=new_data,col="blue")
boxplot(pdays~y,data=new_data,col="maroon")

### Model building ###
#Training#
library(caret)
inTrain<-createDataPartition(y=new_data$y,p=0.7,list=FALSE)
training<-new_data[inTrain,]
testing<-new_data[-inTrain,]
dim(training);dim(testing)
table(training$y);table(testing$y)
library(randomForest)
model<-randomForest(y~.,data=training)
model
# The error level seems to be not good, specially in the "yes" part!

#importance of the predictors
importance(model)

#Testing#
predicted<-predict(model,testing)
table(predicted)
confusionMatrix(predicted,testing$y)
# This phenomenon with the high portion of "yes" misclassification
#is because of the high disproportionate number of the classes.

#Inspite of the correlations being not so high, the accuracy is high 
#because of the combined effect of predictors as well as the power
#of building multiple trees.

#Effect of increasing tree count.

#Let us try to build different number of trees and see the effect of 
#that on the accuracy of the prediction.

accuracy=c()
for (i in seq(1,50,by=1)){
  modFit<-randomForest(y~.,data=training, ntree=i)
  accuracy<-c(accuracy,confusionMatrix(predict(modFit,testing,type="class"),testing$y)$overall[1])
}
par(mfrow=c(1,1))
plot(x=seq(1,50,by=1),y=accuracy,type="l",col="red",
  main="Effect of increasing tree size",xlab="Tree size",ylab="Accuracy")
# This is a good plot to do when you need to know how many trees you need to build.

### Conclusions ###

#Random forests provide better accuracy than plain decision trees because
#of the power of the number of trees built. The example shows that as we 
#increase the number of trees, accuracy also increases. But that comes
#at a cost of increased time and resource usage.

