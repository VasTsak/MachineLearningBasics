# The input data set contains 1000 fatal accidents.It has various variables
# associated with the accident. The goal is to find patterns in the variable
# which accident conditions frequently occur together.

# Techniques used
#Association Rules Mining
#Converting Feature data into Basket Data

##Data Engineering & analysis ##
accident_data<-read.csv("accidents.csv")
str(accident_data)
summary(accident_data)

##Data Transformation ##
#The data frame needs to be converted into a Basket form to be loaded 
#by the arules dataset. The following code does it.

colnames<-names(accident_data)
#Start building a file in basket format - one row per transaction
#and each column value becoming a basket item in format
#<column_name>=<column_value>
basket_str<-""
for (row in 1:nrow(accident_data)){
  if (row !=1){
    basket_str<-paste0(basket_str,"\n")
  }
  basket_str<-paste0(basket_str,",")
  for (col in 2:length(colnames)){
    if (col !=2){
      basket_str<-paste0(basket_str,",")
    }
    basket_str<-paste0(basket_str,colnames[col],"=",accident_data[row,col])
  }
}
write(basket_str,"accident_basket.csv")

## Exploratory Analysis ##
library(arules)
accidents<-read.transactions("accident_basket.csv",sep=",")
summary(accidents)

itemFrequencyPlot(accidents,topN=10,type="absolute",
                  col="darkgreen",horiz=TRUE)

## Modeling & Prediction ##
#We discover the frequently occuring patterns with arules.
rules<-apriori(accidents,parameter=list(supp=0.1,conf=0.3))
inspect(rules[1:40])
