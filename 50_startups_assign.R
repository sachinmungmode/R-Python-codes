install.packages("neuralnet")
library(neuralnet)
library(nnet)
library(plyr)
`50_Startups` <- read.csv("D:/Assignments/Neaural Networks/50_Startups.csv")
View(`50_Startups`)
str(`50_Startups`)

`50_Startups`$State <- as.numeric(revalue(`50_Startups`$State,
                                     c("New York"="0", "California"="1","Florida"="2")))
View(`50_Startups`)
str(`50_Startups`)
attach(`50_Startups`)
# normalize the dataset
normalize <-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(`50_Startups`,FUN=normalize))
summary(Startups_norm$Profit)
head(Startups_norm)

# Training and testing dataset
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
head(Startups_train)
startups_test  <- Startups_norm[ind==2,]
head(startups_test)

# Building neural network
startups_model <- neuralnet(Profit~ R.D.Spend + Administration + Marketing.Spend
                            + State,data = Startups_train)
str(startups_model)
plot(startups_model, rep = "best")
summary(startups_model)
# model evaluation
set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result
cor(predicted_profit,startups_test$Profit)
plot(predicted_profit,startups_test$Profit)
# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(`50_Startups`$Profit)
str_min <- min(`50_Startups`$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
# Improve the model performance
set.seed(12345)
Startups_model2 <- neuralnet(Profit ~ R.D.Spend + Administration + Marketing.Spend
                             + State,data = Startups_train, hidden = 2)
plot(Startups_model2 ,rep = "best")
summary(Startups_model2)
# model evaluation
model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
resul <- data.frame(actual = startups_test$Profit, predicted = predicted_Profit2)
plot(predicted_Profit2,startups_test$Profit)

Startups_model3 <- neuralnet(Profit ~ R.D.Spend + Administration + Marketing.Spend
                             + State,data = Startups_train, hidden = c(3,2))
plot(Startups_model3, rep = "best")
