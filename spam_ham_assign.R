library(caret)
library(MASS)
library(dplyr)
library(pander)
library(wordcloud)
library(tm)
library(e1071) 

sms_raw_NB <- read.csv("D:/Assignments/Naive Base assign/sms_raw_NB.csv")
View(sms_raw_NB)
colnames(sms_raw_NB) <- c("type", "text")
sms_raw_NB$type <- factor(sms_raw_NB$type)                     
str(sms_raw_NB)
table(sms_raw_NB$type)
prop.table(table(sms_raw_NB$type))
# Data Visualization

spam <- subset(sms_raw_NB, type == "spam")
wordcloud(spam$text, max.words = 60, colors = brewer.pal(7, "Paired"), random.order = FALSE)

ham <- subset(sms_raw_NB, type == "ham")
wordcloud(ham$text, max.words = 60, colors = brewer.pal(7, "Paired"), random.order = FALSE)

#Data Pre-processing
sms_corpus <- VCorpus(VectorSource(sms_raw_NB$text))
library(SnowballC)
sms_dtm <- DocumentTermMatrix(sms_corpus, control = 
                                list(tolower = TRUE,
                                     removeNumbers = TRUE,
                                     stopwords = TRUE,
                                     removePunctuation = TRUE,
                                     stemming = TRUE))

dim(sms_dtm)

#Training & Test set
sms_dtm_train <- sms_dtm[1:4447, ]
sms_dtm_test <- sms_dtm[4448:5559, ]

#Training & Test Label
sms_train_labels <- sms_raw_NB[1:4447, ]$type
sms_test_labels <- sms_raw_NB[4448:5559, ]$type

#Proportion for training & test labels
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#Creating Indicator Features
threshold <- 0.1
min_freq = round(sms_dtm$nrow*(threshold/100),0)
min_freq

# Create vector of most frequent words
freq_words <- findFreqTerms(x = sms_dtm, lowfreq = min_freq)
str(freq_words)

#Filter the DTM
sms_dtm_freq_train <- sms_dtm_train[ , freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , freq_words]

dim(sms_dtm_freq_train)
dim(sms_dtm_freq_test)
# converting values to categorical variabbles
convert_values <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_values)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_values)

#Create model from the training dataset
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Make predictions on test set
sms_test_pred <- predict(sms_classifier, sms_test)

#Create confusion matrix
confusionMatrix(data = sms_test_pred, reference = sms_test_labels,
                positive = "spam", dnn = c("Prediction", "Actual"))
