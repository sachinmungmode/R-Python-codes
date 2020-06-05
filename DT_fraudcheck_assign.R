library(C50)
library(tree)
library(gmodels)
library(caret)
Fraud_check <- read.csv("D:/Assignments/Decision tree assign/Fraud_check.csv")
View(Fraud_check)
str(Fraud_check)
# splitting the data
hist(Fraud_check$Taxable.Income)
Risky_Good = ifelse(Fraud_check$Taxable.Income <= 30000, "Risky", "Good")
FC = data.frame(Fraud_check,Risky_Good)
View(FC)
FC_train <- FC[1:300,]
View(FC_train)
FC_test <- FC[301:600,]
View(FC_test)
# building the model using tree function
FC_tree <- tree(Risky_Good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = FC_train)
summary(FC_tree)
plot(FC_tree)
text(FC_tree)
# Predicting the test data using the model
pred_tree_FC <- as.data.frame(predict(FC_tree,newdata=FC_test))
pred_tree_FC["final"] <- NULL
pred_test_FCdf <- predict(FC_tree,newdata=FC_test)
#pred_test_FCdf
pred_tree_FC$final <- colnames(pred_test_FCdf)[apply(pred_test_FCdf,1,which.max)]
pred_tree_FC$final <- as.factor(pred_tree_FC$final)
summary(pred_tree_FC$final)
summary(FC_test$Risky_Good)
mean(pred_tree_FC$final==FC$Risky_Good)
CrossTable(FC_test$Risky_Good,pred_tree_FC$final)
confusionMatrix(FC_test$Risky_Good,pred_tree_FC$final)
