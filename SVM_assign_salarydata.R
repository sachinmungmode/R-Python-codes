library(kernlab)
library(caret)
# training and testing dataset
SalaryData_Train <- read.csv("D:/Assignments/Support vector machine/SalaryData_Train.csv")
View(SalaryData_Train)
SalaryData_Test <- read.csv("D:/Assignments/Support vector machine/SalaryData_Test.csv")
View(SalaryData_Test)
str(SalaryData_Train)
SalaryData_Train$educationno <- as.factor(SalaryData_Train$educationno)
str(SalaryData_Test)
SalaryData_Test$educationno <- as.factor(SalaryData_Test$educationno)

# model building
model_vanila <- ksvm(SalaryData_Train$Salary ~., data = SalaryData_Train, 
                     kernel = "vanilladot")
model_vanila
Salary_prediction <- predict(model_vanila, SalaryData_Test)
table(Salary_prediction,SalaryData_Test$Salary)
mean(Salary_prediction == SalaryData_Test$Salary)

# rbfdot- kernel
model_rbf <- ksvm(SalaryData_Train$Salary ~., data = SalaryData_Train, 
                     kernel = "rbfdot")
model_rbf
Salary_prediction1 <- predict(model_rbf, SalaryData_Test)
table(Salary_prediction1, SalaryData_Test$Salary)
mean(Salary_prediction1 == SalaryData_Test$Salary)

# tanhdot - kernel
model_tanh <- ksvm(SalaryData_Train$Salary ~., data = SalaryData_Train, 
                  kernel = "tanhdot")
model_tanh
Salary_prediction2 <- predict(model_tanh, SalaryData_Test)
table(Salary_prediction2, SalaryData_Test$Salary)
mean(Salary_prediction2 == SalaryData_Test$Salary)

# laplacedot- kernel
model_laplace <- ksvm(SalaryData_Train$Salary ~., data = SalaryData_Train, 
                   kernel = "laplacedot")
model_laplace
Salary_prediction3 <- predict(model_laplace, SalaryData_Test)
table(Salary_prediction3, SalaryData_Test$Salary)
mean(Salary_prediction3 == SalaryData_Test$Salary)
