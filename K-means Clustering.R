#Techniques used
#K-means clustering 
#Centering and scaling

#Data engineering & analysis#
auto_data<-read.csv("auto-data.csv")
str(auto_data)
summary(auto_data)
head(auto_data)
#Data cleansing#
#The ranges of values in each of the variables look ok , no outliers.
#Clustering needs all numeric values to be in the same range.
#Hence we need to center and scale the data set.
scaled_num<-scale( auto_data[8:12])
auto_data[,8:12]<-scaled_num
summary(auto_data)

#Exploratory Analysis# 
par(mfrow=c(1,5))
boxplot(auto_data$HP,col="red")
title("HP")

boxplot(auto_data$RPM,col="blue")
title("RPM")

boxplot(auto_data$MPG.CITY,col="green")
title("MPG.CITY")

boxplot(auto_data$MPG.HWY,col="maroon")
title("MPG.HWY")

boxplot(auto_data$PRICE,col="cyan")
title("PRICE")
#We choose not to remove the outliers because they are many , so they 
# may not be outliers

## Model and prediction ##

#Build Clusters for 2 variables and 100 observations.
library(class)
#keep the same seed for each execution. Seed impacts the initial centroid
#position and hence may impact the actual cluster format. 
set.seed(1111)
auto_subset<-auto_data[1:100,c(8,12)]
clusters<-kmeans(auto_subset,4)
clusters
#The higher the percentage of (sum of squares by cluster) the better.
par(mfrow=c(1,1))
plot(auto_subset$HP,auto_subset$PRICE,col=clusters$cluster,pch=20,cex=2)
points(clusters$centers,col="purple",pch=17,cex=3)
# This plot shows the clusters !

## Clustering for all the data ##
# First convert all the factors into numeric
for ( i in 1:8){
  auto_data[,i]=as.numeric(auto_data[,i])
}
summary(auto_data)

set.seed(1111)
clusters<-kmeans(auto_data[,7:12],4) # Cluster the rest of the data
clusters

#The greatest challenge , is the number of Clusters that I need!

## FInding optimal Number of Clusters ##
wssplot<-function(data,nc=15,seed=1234){ #It nc=15,it will try from 1 to 15 clusters
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)} # It will plot the withinss value
  plot(1:nc,wss,type="b",xlab="Number of Clusters",
       ylab="Within groups sum of squares",col="red")}
wssplot(auto_data)
