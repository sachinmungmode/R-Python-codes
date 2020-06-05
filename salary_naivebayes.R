install.packages("naivebayes")
library(naivebayes)
library(ggplot2)
library(caret)
library(psych)
library(e1071)

SalaryData_Train <- read.csv("D:/Assignments/Naive Base assign/SalaryData_Train.csv")
View(SalaryData_Train)
SalaryData_Train$educationno <- as.factor(SalaryData_Train$educationno)
str(SalaryData_Train)

SalaryData_Test <- read.csv("D:/Assignments/Naive Base assign/SalaryData_Test.csv")
View(SalaryData_Test)
SalaryData_Test$educationno <- as.factor(SalaryData_Test$educationno)
str(SalaryData_Test)

describe(SalaryData_Train)
describe(SalaryData_Test)

install.packages("Amelia")
library(Amelia)
missmap(SalaryData_Train)

ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$age, fill = SalaryData_Train$Salary)) +
  geom_boxplot() + ggtitle("Box Plot")
plot(SalaryData_Train$workclass,SalaryData_Train$Salary)
plot(SalaryData_Train$education,SalaryData_Train$Salary)
plot(SalaryData_Train$educationno,SalaryData_Train$Salary)
plot(SalaryData_Train$maritalstatus,SalaryData_Train$Salary)
plot(SalaryData_Train$occupation,SalaryData_Train$Salary)       
plot(SalaryData_Train$relationship,SalaryData_Train$Salary)
plot(SalaryData_Train$race,SalaryData_Train$Salary)
plot(SalaryData_Train$sex,SalaryData_Train$Salary)
ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$capitalgain, fill = SalaryData_Train$Salary)) + geom_boxplot()
ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$capitalloss, fill = SalaryData_Train$Salary)) + geom_boxplot()
ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$hoursperweek, fill = SalaryData_Train$Salary)) + geom_boxplot()
plot(SalaryData_Train$native,SalaryData_Train$Salary)

# Naive Bayes model
Salary_model <- naiveBayes(SalaryData_Train$Salary~., data = SalaryData_Train)
Salary_model

# prediction model
Salary_pred <- predict(Salary_model,SalaryData_Test)
Salary_pred
mean(Salary_pred == SalaryData_Test$Salary)

confusionMatrix(Salary_pred,SalaryData_Test$Salary)


