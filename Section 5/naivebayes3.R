#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute


library(tm)
library(e1071)

#The data can be obtained here: https://inclass.kaggle.com/c/adcg-ss14-challenge-02-spam-mails-detection/data
setwd("/Users/romeokienzler/Documents/romeo/Dropbox/arbeit/r/rkurs/lecture3/spamclassification/TR/")
labels = read.csv('/Users/romeokienzler/Documents/romeo/Dropbox/arbeit/r/rkurs/lecture3/spamclassification/spam-mail.tr.label')
labels = labels[,2]
#get a list of all email from the directory
file_list = list.files()

#remove the first entry (doesn't contain an email)
file_list = file_list[-1]

#create an empty data frame
df = data.frame(emailtext=character(),stringsAsFactors=FALSE) 

#add each email as row to the data frame
for (fileName in file_list){
  text=readChar(fileName, file.info(fileName)$size)
  df=rbind(df,data.frame(text,stringsAsFactors = FALSE))
}

#we have now 2500 emails in the data frame
dim(df)


dtm=DocumentTermMatrix(Corpus(VectorSource(df$text)))
dim(dtm)
inspect(dtm[2380:2385, 100000:100005])

#load labels from separate file
labels

five_times_words <- findFreqTerms(sms_dtm_train, 5)
length(five_times_words)

sms_train <- DocumentTermMatrix(spam_corpus_train, control=list(dictionary = five_times_words))

sms_test <- DocumentTermMatrix(spam_corpus_test, control=list(dictionary = five_times_words))


convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

dtm_convert <- apply(dtm, 2, convert_count)
classifier <- naiveBayes(dtm_convert[1:1250,], factor(labels[1:1250]))
class(classifier)
pred <- predict(classifier, newdata=dtm_convert[1251:2500,])
table(pred, labels[1251:2500])
truthVector = pred == labels[1251:2500]
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
good/(good+bad)


