install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('ggplot2')  #for Data Visualization
install.packages('class')    #KNN 
install.packages('caret')    #Confusion Matrix
install.packages('corrplot') #Correlation Plot
library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <- read.csv("D:/Assignments/KNN assign/glass.csv")
View(glass)
str(glass)
table(glass$Type)
#standardize the data
standard.features <- scale(glass[,1:9])
glass_data <- cbind(standard.features,glass[10])
head(glass_data)
corrplot(cor(glass_data))

# train and test data
set.seed(123)
glass_index <- sample(2, nrow(glass_data), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_data[glass_index==1,]
glass_train
glass_test <-  glass_data[glass_index==2,]
glass_test
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_train_labels
glass_test_labels <-  glass[ind1==2,10]
glass_test_labels
# KNN model and choosing k value
glass_test_pred1 <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=1)
confusionMatrix(table(glass_test_pred1,glass_test_labels))

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
glass_test_pred
table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels)
confusionMatrix(table(glass_test_pred,glass_test_labels))


