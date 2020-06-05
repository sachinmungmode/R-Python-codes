library(randomForest)
library(caret)
Company_Data <- read.csv("D:/Assignments/Random forest assign/Company_Data.csv")
View(Company_Data)
str(Company_Data)
hist(Company_Data$Sales, main = "Sales of Company",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
highsales = ifelse(Company_Data$Sales < 9, "No", "Yes")  # if greater than 9 then high sales else Low
CD = data.frame(Company_Data[2:11], highsales)
str(CD)
table(CD$highsales)
# splitting the data in train and test dataset
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
CD_train <- CD[ind==1,]
CD_test  <- CD[ind==2,]
# building the RF model and prediction model
set.seed(213)
rf <- randomForest(highsales~., data=CD_train)
rf
# prediction on training dataset
CD_pred1 <- predict(rf, CD_train)
head(CD_pred1)
head(CD_train$highsales)
confusionMatrix(CD_pred1, CD_train$highsales)
# prediction on testing dataset
CD_pred2 <- predict(rf,CD_test)
confusionMatrix(CD_pred2, CD_test$highsales)
plot(rf)
legend("topright", colnames(rf$err.rate),col= 1:3,cex = 0.8,fill = 1:3)

# Tune Random Forest Model mtry 
tune <- tuneRF(CD_train[,-11], CD_train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(highsales~., data=CD_train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred3 <- predict(rf1, CD_train)
confusionMatrix(pred3, CD_train$highsales)
pred4 <- predict(rf1, CD_test)
confusionMatrix(pred4, CD_test$highsales)

# variable importance
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5- Variable Importance")
