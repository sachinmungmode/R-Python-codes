library(caret)
library(C50)
library(tree)
library(gmodels)
Company_Data <- read.csv("D:/Assignments/Decision tree assign/Company_Data.csv")
View(Company_Data)
str(Company_Data)
# splitting the data based on Sales
hist(Company_Data$Sales)
High = ifelse(Company_Data$Sales<10, "No", "Yes")
CD = data.frame(Company_Data, High)
CD <- CD[,2:12]
View(CD)
CD_train <- CD[1:200,]
head(CD_train)
CD_test <- CD[201:400,]
head(CD_test)

# building a model using tree function
cd_tree <- tree(High~.,data=CD_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree)
# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(CD_test$High)
mean(pred_tree$final==CD$High)
CrossTable(CD_test$High,pred_tree$final)
confusionMatrix(CD_test$High,pred_tree$final)
