install.packages("kernlab")
library(kernlab)
library(caret)
forestfires <- read.csv("D:/Assignments/Support vector machine/forestfires.csv")
View(forestfires)
str(forestfires)
class(forestfires)
summary(forestfires)
# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires$temp = normalize(forestfires$temp)
forestfires$RH   = normalize(forestfires$RH)
forestfires$wind = normalize(forestfires$wind)
forestfires$rain = normalize(forestfires$rain)
# data partition
set.seed(123)
ind <- sample(2, nrow(forestfires), replace = TRUE, prob = c(0.7,0.3))
Forestfires_train <- forestfires[ind==1,]
Forestfires_test  <- forestfires[ind==2,]

# Model building
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= Forestfires_train,kernel = "vanilladot")
model1
area_pred <- predict(model1, Forestfires_test)
table(area_pred, Forestfires_test$size_category)
agreement <- area_pred == Forestfires_test$size_category
table(agreement)
mean(area_pred == Forestfires_test$size_category)

# rbfdot
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= Forestfires_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot, newdata = Forestfires_test)
mean(pred_rfdot==Forestfires_test$size_category)

# kernel= besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= Forestfires_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata = Forestfires_test)
mean(pred_bessel == Forestfires_test$size_category)

# kernel = polydot
model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= Forestfires_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = Forestfires_test)
mean(pred_poly == Forestfires_test$size_category)
