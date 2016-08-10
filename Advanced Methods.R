# The "caret" package is a set of functions that attempt to streamline
#the process for creating predictive models.
#Tools : 
#Data splitting
#Pre-processing
#Feature Selection
#Model tuning using resampling
#Variable importance estimation

##Techniques used ###
#Principali component analysis
#Training and Testing 
#Confusion Matrix
#Neural Networks
#Support Vector Machines
#Bagging
#Boosting

## Data Engineering & Analysis ## 
cancer_data<-read.csv("breast_cancer.csv")
str(cancer_data)
summary(cancer_data)

## Exploratory Data Analysis Correlations ##
library(psych)

#Given the large number of variables , split into 3 sets and see
#correlation and diagnosis.
pairs.panels(cancer_data[,c(2,3:10)])
pairs.panels(cancer_data[,c(2,11:20)])
pairs.panels(cancer_data[,c(2,21:32)])

#Principal Component Analysis
#We first scale the data and discover the principal components of the 
#data.Then we only pick the top components that have the heaviest 
#influence on the target.

#Scale the data
scaled_data<-scale(cancer_data[,3:32])
#conver into principal components
pca_data<-prcomp(scaled_data)

plot(pca_data)
summary(pca_data)

#Get only the first 3 components
final_data<-data.frame(pca_data$x[,1:3])
#add diagnosis to the data frame
final_data$diagnosis<-cancer_data$diagnosis
pairs.panels(final_data)
#The first 3 principal components influences the 75% of the target,
#so we only pick the top 3. They have good correlation to the target
#But no correlation between themselves!

## Modeling and Prediction ##
library(caret)
inTrain<-createDataPartition(y=final_data$diagnosis,p=0.7,list=FALSE)
training<-final_data[inTrain,]
testing<-final_data[-inTrain,]

dim(training);dim(testing)
table(training$diagnosis);table(testing$diagnosis)

## Model Building and Testing
#We will build different models based on 4 different algorithms. Then
#we predict on the test data and measure accuracy. Finally, we compare
#the algorithms for modelling and prediction. 

predlist<-c("bagFDA", #bagging
           "LogitBoost", #boosting
           "nnet", #Neural Networks
           "svmRadialCost") #SVM)

#Create a result data set.
results<-data.frame(Algorithm=character(),Duration=numeric(),Accuracy=numeric(),
                    stringsAsFactors = FALSE)

#Loop through algorithm list and perform model building and prediction

for (i in 1:length(predlist)){
  pred<-predlist[i]
  print(paste("Algorithm =",pred))
  #Measure Time
  startTime<-as.integer(Sys.time())
}

#Loop through algorithm list and perform model building and prediction

for (i in 1:length(predlist)){
  pred<-predlist[i]
  print(paste("Algorithm=" ,pred))
  #Measure Time
  startTime<-as.integer(Sys.time())
  #Build model
  model<-train(diagnosis~.,data=training,method=pred)
  #Predict
  predicted<-predict(model,testing)
  #Compare results
  matrix<-confusionMatrix(predicted,testing$diagnosis)
  #Measure End time
  endTime<-as.integer(Sys.time())
  #Store result
  thisresult<-c(as.character(pred),endTime-startTime,as.numeric(matrix$overall[1]))
  results[i,1]<-pred
  results[i,2]<-endTime-startTime
  results[i,3]<-round(as.numeric(matrix$overall[1]*100,2))
}
# Print results
results

### Conclusions ### 
#Given that there is one main principal component PC1, most algorithms
#will perform with excellent accuracy. This example shows how large predictors
#can be easily compressed using PCA and then used for prediction.