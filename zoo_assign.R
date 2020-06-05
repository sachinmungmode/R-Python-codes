library(ggplot2)
library(class)
library(caret)
Zoo <- read.csv("D:/Assignments/KNN assign/Zoo.csv")
View(Zoo)
str(Zoo)
# Split dataset in training and test dataset
Zoo.train <- Zoo[1:70,2:17]
Zoo.test <- Zoo[71:101,2:17]
# Splitting last column whcih contains animal class types
Zoo.train.labels <- Zoo[1:70,18]
Zoo.test.labels <- Zoo[71:101,18]
# Applying KNN model on the training data keeping k value 9
ZooClassification <- knn(train = Zoo.train,test = Zoo.test,cl=Zoo.train.labels,k=9)
ZooClassification
confusionMatrix(table(ZooClassification,Zoo.test.labels))

# k=7
ZooClassification7 <- knn(train = Zoo.train,test = Zoo.test,cl=Zoo.train.labels,k=7)
confusionMatrix(table(ZooClassification7,Zoo.test.labels))

plot(ZooClassification, col = rainbow(7),main="Classification of Animals",xlab="Types of Animals")

