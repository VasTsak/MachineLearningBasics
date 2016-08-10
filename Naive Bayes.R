#The input data  is a set of SMS messages that has been classified
#as either "ham" or "spam". The goal is to build a model to identify
#messages as either "ham" or "spam".

#Techniques to be used:
#Naive Bayes Classifier
#Training and Testing
#Confusion Matrix
#Text Pre-Processing

### Data Engineering & Analysis ###
#Loading and understanding the data
sms_data<- read.csv("sms_spam.csv",stringsAsFactors = FALSE)
sms_data$type<-as.factor(sms_data$type)
str(sms_data)

#Data Cleansing
library(tm)
library(NLP)

#Create Corpus for the message
mesg_coprus<-Corpus(VectorSource(sms_data$text))
inspect(mesg_coprus)

#Cleanse the data
#Remove punctuation marks 
refined_corpus<-tm_map(mesg_coprus,removePunctuation)
#Remove white space
refined_corpus<-tm_map(mesg_coprus,stripWhitespace)
#Convert to lower Case
refined_corpus<-tm_map(mesg_coprus,content_transformer(tolower))
#Remove numbers in text
refined_corpus<-tm_map(mesg_coprus,removeNumbers)
#Remove stop words
refined_corpus<-tm_map(mesg_coprus,removeWords,stopwords())
#Remove specific words
refined_corpus<-tm_map(refined_corpus,removeWords,c("else","the","are",
 "has","they","as","a","his","on","when","is","in","already" ))

#Look at the processed data 
inspect(refined_corpus[1:5])

#Create a document-term sparse matrix
dtm<-DocumentTermMatrix(refined_corpus)
dtm
dim(dtm) #every row is a message, every column is a word

#Focus on the important words
#So remove the ones that have been used few times ( let's say 10)
filtered_dtm<-DocumentTermMatrix(refined_corpus,list(dictionary=findFreqTerms(dtm,10)))
dim(filtered_dtm)
# Doesn't it feel better?

#Inspect the contents be converting it into a matrix and transposing it
t(inspect(filtered_dtm)[1:25,])

### Exploratory Analysis ###
library(wordcloud)
pal<-brewer.pal(9,"Dark2")
wordcloud(refined_corpus[sms_data$type=="ham"],min.freq = 5,
          random.order = FALSE,colors = pal)
wordcloud(refined_corpus[sms_data$type=="spam"],min.freq = 2,
          random.order = FALSE,colors = pal)

### Modeling and Prediction ### 
library(caret)
inTrain<-createDataPartition(y=sms_data$type,p=0.7,list=FALSE)

# Split the raw data
train_raw <-sms_data[inTrain,]
test_raw <- sms_data[-inTrain,]

# Split the Corpus 
train_corpus<-refined_corpus[inTrain]
test_corpus<-refined_corpus[-inTrain]

#Split the dtm
train_dtm<-filtered_dtm[inTrain,]
test_dtm<-filtered_dtm[-inTrain,]

#Instead of using the counts of words within document, we will replace
#them with indicators "Yes" or "No".
#"Yes" indicates if the word occured in the document and "No" indicate
#it doesn't. This procedure converts numeric data into factor data.

conv_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c("No","Yes"))
}
train <- apply(train_dtm,MARGIN = 2,conv_counts)
test <- apply(test_dtm,MARGIN = 2,conv_counts)

#Convert to a data frame and add the target variable 
df_train <-as.data.frame(train)
df_test <-as.data.frame(test)

df_train$type<-train_raw$type
df_test$type <-test_raw$type
df_train[1:10,1:10]
# You will seee that some rows are missing, this is because they are 
# in the testing set

#Model building
library(e1071)

#Leave behind the target variable
modFit <-naiveBayes(df_train[,-60],df_train$type)
modFit

#Testing 
#Now let's predict the class for each sample in the test data.
#Then compare the prediction with the actual value of the class.
predictions<- predict(modFit,df_test)
confusionMatrix(predictions,df_test$type)
# Great accuracy! 