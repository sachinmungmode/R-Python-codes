library(randomForest)
library(caret)
Fraud_check <- read.csv("D:/Assignments/Random forest assign/Fraud_check.csv")
View(Fraud_check)
str(Fraud_check)

hist(Fraud_check$Taxable.Income, main = "Taxable income", col = c("blue","red", "green","violet"))
Risky_Good = ifelse(Fraud_check$Taxable.Income <= 30000, "Risky", "Good")
FC = data.frame(Fraud_check,Risky_Good)
View(FC)
str(FC)
table(FC$Risky_Good)
# splitting the data
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
FC_train <- FC[ind==1,]
FC_test  <- FC[ind==2,]
# building the model
set.seed(321)
rf <- randomForest(Risky_Good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = FC_train)
rf
# prediction on training dataset
FC_pred1 <- predict(rf, FC_train)
head(FC_pred1)
head(FC_train$Risky_Good)
confusionMatrix(FC_pred1, FC_train$Risky_Good)
# prediction on testing dataset
FC_pred2 <- predict(rf,FC_test)
confusionMatrix(FC_pred2, FC_test$Risky_Good)
plot(rf)
legend("topright", colnames(rf$err.rate),col= 1:3,cex = 0.8,fill = 1:3)
# Tune Random Forest Model mtry 
tune <- tuneRF(FC_train[,-7], FC_train[,7], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(Risky_Good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = FC_train, ntree = 300, mtry = 2, 
                    importance = TRUE, proximity = TRUE)
rf1
pred3 <- predict(rf1, FC_train)
confusionMatrix(pred3, FC_train$Risky_Good)
pred4 <- predict(rf1, FC_test)
confusionMatrix(pred4, FC_test$Risky_Good)

# variable importance
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5- Variable Importance")
