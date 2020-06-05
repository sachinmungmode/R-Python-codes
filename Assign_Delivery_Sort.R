View(delivery_time)
attach(delivery_time)
## Visualizing data using different plot methods
scatter.smooth(Sorting.Time, Delivery.Time, main = "Delivery ~ Sorting")
par(mfrow=c(1, 2))
boxplot(Delivery.Time, main = "Delivery_Time")
boxplot(Sorting.Time, main = "Sorting_Time")
par(mfrow=c(1, 2))
plot(density(Delivery.Time), main = "Density: D_Time", ylab = "Frequency")
plot(density(Sorting.Time), main = "Density: S_Time", ylab = "Frequency")
## check Correlation
cor(Sorting.Time,Delivery.Time)
## linear model creation
linearmod <- lm(Delivery.Time~Sorting.Time)
summary(linearmod)
linearmod$residuals
pred <- predict(linearmod, interval = "predict")
summary(pred)
pred <- as.data.frame(pred)
pred
AIC(linearmod) # Calculate akaike information criterion
BIC(linearmod) #Bayesian information criterion
## logarithmic model
linearmod_log <- lm(Delivery.Time ~ log(Sorting.Time))
summary(linearmod_log)
## p-value for delivery time is greater than 0.05, so we fail to reject null hypothesis.

## Exponential model
linearmod_exp <- lm(log(Delivery.Time) ~ Sorting.Time)
summary(linearmod_exp)
pred_e <- predict(linearmod_exp, interval = "predict")
DT <- exp(pred_e)
summary(DT)
DT_e <- as.data.frame(DT)
summary(DT_e)
AIC(linearmod_exp)
BIC(linearmod_exp)
## create training and test data
trainingRowIndex <- sample(1:nrow(delivery_time), 0.8*nrow(delivery_time))  # row indices for training data
trainingData <- delivery_time[trainingRowIndex, ]  # model training data
testData  <- delivery_time[-trainingRowIndex, ]   # test data
# Build the model on training data
lmMod <- lm(Delivery.Time ~ Sorting.Time, data=trainingData)  # build the model
summary(lmMod)
AIC(lmMod)
BIC(lmMod)
deli_Pred <- predict(lmMod, testData)  # predict delivery time
summary(deli_Pred)
deli_Pred

del_exp_t <- lm(log(Delivery.Time)~Sorting.Time, data = trainingData)
summary(del_exp_t)
AIC(del_exp_t)
BIC(del_exp_t)
del_ex_pred <- predict(del_exp_t, testData)
DT_ep <- exp(del_ex_pred)
summary(DT_ep)
DT_ep

