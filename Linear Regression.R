setwd("C:\\Users\\vasil\\Desktop\\udemy\\R Data Science")
str(mtcars)  ##### "?" when there is a missing value

### Data engineering and Data Analysis###

summary(mtcars) # Quartiles
head(mtcars) # top 6 cars

# Missing value handling
mtcars$hp<- as.numeric(mtcars$hp) # To get rid of "?" and turn it into "NA"
mtcars$hp[is.na(mtcars$hp)] <- mean(mtcars$hp,na.rm=TRUE) # To replace the missing values with the mean
summary(mtcars)

### Exploratory Analysis ###

library(ggplot2)
ggplot(mtcars,aes(factor(cyl),mpg))+
  geom_boxplot(aes(fill=factor(cyl)))

## Correlations-Dimensionality reduction##
library(psych)
pairs.panels(mtcars)
mtcars$cyl<-NULL #High correlation, so we remove them
mtcars$disp<-NULL #High correlation, so we remove them
summary(mtcars)

###Modeling and prediction###

lm_model<-lm(mpg~.,mtcars)
summary(lm_model)

predicted<- predict.lm(lm_model,mtcars)
summary(predicted)
#plot predicted vs the actuals 

plot(mtcars$mpg, predicted ,col="red")
cor(mtcars$mpg,predicted) # GREAT prediction

