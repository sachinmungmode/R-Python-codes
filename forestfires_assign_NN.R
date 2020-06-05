install.packages("neuralnet")
library(neuralnet)
library(nnet)
library(plyr)
forestfires <- read.csv("D:/Assignments/Neaural Networks/forestfires.csv")
View(forestfires)
str(forestfires)
forestfires$month <- as.numeric(as.factor(forestfires$month))
forestfires$day <- as.numeric(as.factor(forestfires$day))
forestfires$size_category <- as.numeric(as.factor(forestfires$size_category))
View(forestfires)
# normalize the data
normalize <-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm <- as.data.frame(lapply(forestfires[,1:11],FUN=normalize))
summary(forestfires_norm)
head(forestfires_norm)
forestfires_norm1 <- as.data.frame(cbind(forestfires_norm,forestfires$size_category))
head(forestfires_norm1)

# Training and testing dataset
set.seed(123)
ind <- sample(2, nrow(forestfires_norm1), replace = TRUE, prob = c(0.7,0.3))
forestfirenorm1_train <- forestfires_norm1[ind==1,]
head(forestfirenorm1_train)
forestfirenorm1_test  <- forestfires_norm1[ind==2,]
head(forestfirenorm1_test)

# Building Neural network model
area_burned <- neuralnet(area~ month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = forestfirenorm1_train)
str(area_burned)
plot(area_burned, rep = "best")
#model evaluation
set.seed(12300)
model_results <- compute(area_burned,forestfirenorm1_test[1:10])
predicted_area <- model_results$net.result
cor(predicted_area,forestfirenorm1_test$area)
plot(predicted_area,forestfirenorm1_test$area)

# de-normalization of dataset
str_max <- max(forestfires$area)
str_min <- min(forestfires$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
Actual_forestfire_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actual_forestfire_pred)

# improve model performance
area_burned1 <- neuralnet(area~ month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = forestfirenorm1_train, hidden = c(5,2))
plot(area_burned1,rep = "best")

area_burned2 <- neuralnet(area~ month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = forestfirenorm1_train, hidden = c(7,4))
plot(area_burned2,rep = "best")
model_result_2 <- compute(area_burned2,forestfirenorm1_test[1:10])
predicted_area2 <- model_result_2$net.result
cor(predicted_area2,forestfirenorm1_test$area)
