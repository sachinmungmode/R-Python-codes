View(`50_Startups`)
attach(`50_Startups`)
# Exploratory data analysis
# 1.Measures of central tendency
colMeans(`50_Startups`[sapply(`50_Startups`, is.numeric)])
median(R.D.Spend) 
median(Administration)
median(Marketing.Spend)
median(Profit)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(R.D.Spend)
getmode(Administration)
getmode(Marketing.Spend)
getmode(Profit)
# 2. Measures of dispersion
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)
range(R.D.Spend)
range(Administration)
range(Marketing.Spend)
range(Profit)
# 3. Skewness- 3rd business moment
library(moments)
skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)
# 4. Kurtosis - 4th business moment
kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Profit)
# Graphical Representations
par(mfrow=c(1, 2))
qqnorm(R.D.Spend,main = "R & D"); qqline(R.D.Spend, col = "blue")
qqnorm(Administration,main = "Administration"); qqline(Administration, col = "blue")
qqnorm(Marketing.Spend,main = "Marketing.Spend"); qqline(Marketing.Spend, col = "blue")
qqnorm(Profit,main = "Profit"); qqline(Profit, col = "blue")

par(mfrow=c(1, 2))
boxplot(R.D.Spend,main = "R & D Spending", col = "orange")
boxplot(Administration,main = "Administration spending", col = "orange")
par(mfrow=c(1, 2))
boxplot(Marketing.Spend,main = "Marketing Spending", col = "orange")
boxplot(Profit,main = "Profit", col = "orange")

par(mfrow=c(1, 2))
hist(R.D.Spend,main = "R & D Spending", col = "orange")
hist(Administration,main = "Administration spending", col = "orange")
par(mfrow=c(1, 2))
hist(Marketing.Spend,main = "Marketing Spending", col = "orange")
hist(Profit,main = "Profit", col = "orange")

# Correlation coefficient matrix for strength and direction
pairs(`50_Startups`)
cor(`50_Startups`[sapply(`50_Startups`,is.numeric)])
# multi linear regression model
# using all predictor variables
model.profit <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend,data = `50_Startups`)
summary(model.profit)
summary(model.profit)$coefficients
# model using single predictor
model.profitRD <- lm(Profit ~ R.D.Spend , data=`50_Startups`)
summary(model.profitRD)
model.profitADM <- lm(Profit ~ Administration, data = `50_Startups`)
summary(model.profitADM)
model.profitMS <- lm(Profit ~ Marketing.Spend, data = `50_Startups`)
summary(model.profitMS)
model.profitRDM <- lm(Profit ~ R.D.Spend + Marketing.Spend, data=`50_Startups`)
summary(model.profitRDM)
model.profitRDAD <- lm(Profit ~ R.D.Spend + Administration, data=`50_Startups`)
summary(model.profitRDAD)
## RSE factor
sigma(model.profit)/mean(`50_Startups`$Profit)
sigma(model.profitRDM)/mean(`50_Startups`$Profit)
sigma(model.profitRDAD)/mean(`50_Startups`$Profit)
# prediction model
pred_profit <- predict(model.profitRDM, interval = "predict")
pred_profitRDM <- as.data.frame(pred_profit)
pred_profitRDM
