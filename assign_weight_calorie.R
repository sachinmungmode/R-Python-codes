calories_consumed <- read.csv("C:/Users/91909/Downloads/Data+sets+of+slr/Data sets of slr/calories_consumed.csv")
View(calories_consumed)
## Visualizing data using different plot methods
scatter.smooth(Calories.Consumed, Weight.gained..grams., main = "WeightGained ~ CaloriesConsumed")
par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(Calories.Consumed, main = "Calorie Consumption")
boxplot(Weight.gained..grams., main = "Weight Gain")
par(mfrow=c(1, 2))
plot(density(Calories.Consumed), main = "Density: Calorie", ylab = "Frequency")
plot(density(Weight.gained..grams.), main = "Density: Weight", ylab = "Frequency")
## check Correlation
cor(Calories.Consumed,Weight.gained..grams.)
## linear model creation
linearModel <- lm(Weight.gained..grams.~Calories.Consumed, data = calories_consumed)
summary(linearModel)
linearModel$residuals
predict(linearModel)
pred <- predict(linearModel, interval = "predict")
summary(pred)
pred <- as.data.frame(pred)
pred
AIC(linearModel)
BIC(linearModel)
## logarithmic model
linMod_log <- lm(Weight.gained..grams.~ log(Calories.Consumed), data = calories_consumed)
summary(linMod_log)
predict(linMod_log)
linMod_log$residuals
pred2 <- predict(linMod_log, interval = "predict")
summary(pred2)
pred2 <- as.data.frame(pred2)
pred2
AIC(linMod_log)
BIC(linMod_log)

## Exponential Model
reg_exp <- lm(log(Weight.gained..grams.) ~ Calories.Consumed)
summary(reg_exp)
reg_exp$residuals
pred_exp <- predict(reg_exp,interval = "predict")
WG <- exp(pred_exp)
summary(WG)
pred_exp <- as.data.frame(pred_exp)
pred_exp
AIC(reg_exp)
BIC(reg_exp)

## create training and test data
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(calories_consumed), 0.8*nrow(calories_consumed))  # row indices for training data
trainingData <- calories_consumed[trainingRowIndex, ]  # model training data
testData  <- calories_consumed[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(Weight.gained..grams.~ Calories.Consumed, data=trainingData)  # build the model
summary(lmMod)
distPred <- predict(lmMod, testData)  # predict weight gained
summary(distPred)
AIC(lmMod)
BIC(lmMod)

reg_exp_t <- lm(log(Weight.gained..grams.)~Calories.Consumed, data = trainingData)
summary(reg_exp_t)
distpred_e <- predict(reg_exp_t, testData)
summary(distpred_e)
AIC(reg_exp_t)
BIC(reg_exp_t)
