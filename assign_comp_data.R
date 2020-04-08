Computer_Data <- read.csv("D:/Assignments/Assign 4 MLR/Computer_Data.csv")
View(Computer_Data)
attach(Computer_Data)
# categorical data to numeric data conversion
cd <- ifelse(Computer_Data$cd == "yes",1,0)
cd <- data.frame(cd)
View(cd)
multi <- ifelse(Computer_Data$multi == "yes",1,0)
multi <- data.frame(multi)
View(multi)
premium <- ifelse(Computer_Data$premium == "yes",1,0)
premium <- data.frame(premium)
View(premium)
Computer_Data <- cbind(price,speed,hd,ram,screen,cd,multi,premium,ads,trend)
View(Computer_Data)

# Exploratory data analysis
# 1.Measures of central tendency
colMeans(Computer_Data[sapply(Computer_Data, is.numeric)])
median(Computer_Data$price);median(Computer_Data$speed);median(Computer_Data$hd);median(Computer_Data$ram)
median(Computer_Data$screen);median(Computer_Data$cd);median(Computer_Data$multi);median(Computer_Data$premium)
median(Computer_Data$ads);median(Computer_Data$trend)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#attach(Computer_Data)
getmode(price);getmode(speed);getmode(hd);getmode(ram);getmode(screen)
getmode(Computer_Data$cd);getmode(Computer_Data$multi);getmode(Computer_Data$premium);getmode(ads);getmode(trend)
# 2. Measures of dispersion
var(price);var(speed);var(hd);var(ram);var(screen)
var(Computer_Data$cd);var(Computer_Data$multi);var(Computer_Data$premium);var(ads);var(trend)
sd(price);sd(speed);sd(hd);sd(ram);sd(screen);sd(Computer_Data$cd)
sd(Computer_Data$multi);sd(Computer_Data$premium);sd(ads);sd(trend)
range(price);range(speed);range(hd);range(ram);range(screen);range(Computer_Data$cd)
range(Computer_Data$multi);range(Computer_Data$premium);range(ads);range(trend)
# Skewness
library(moments)
skewness(price);skewness(speed);skewness(hd);skewness(ram);skewness(screen);skewness(Computer_Data$cd)
skewness(Computer_Data$multi);skewness(Computer_Data$premium);skewness(ads);skewness(trend)
# Kurtosis
kurtosis(price);kurtosis(speed);kurtosis(hd);kurtosis(ram);kurtosis(screen);kurtosis(Computer_Data$cd)
kurtosis(Computer_Data$multi);kurtosis(Computer_Data$premium);kurtosis(ads);kurtosis(trend)
# Graphical representation
par(mfrow=c(2,5))
for (i in 1:ncol(Computer_Data[,1: ncol(Computer_Data) ])){  
  qqnorm(Computer_Data[, i], main = names(Computer_Data[i]))
  qqline(Computer_Data[, i])
}
par(mfrow=c(2,5))
for (i in 1:ncol(Computer_Data[,1: ncol(Computer_Data) ])){  
  boxplot(Computer_Data[, i], main = names(Computer_Data[i]))
}
par(mfrow=c(2,5))
for (i in 1:ncol(Computer_Data[,1: ncol(Computer_Data) ])){  
  hist(Computer_Data[, i], main = names(Computer_Data[i]))
}
# Correlation coefficient
cor(Computer_Data[sapply(Computer_Data,is.numeric)])
#multilinear regression model
model.price <- lm(price~ speed+hd+ram+screen+Computer_Data$cd+Computer_Data$premium+ads+trend)
summary(model.price)
sigma(model.price)/mean(Computer_Data$price)##12.43 percent error
library(car)
vif(model.price)
model.price1 <- lm(price ~ speed+hd+ram+screen, data = Computer_Data)
summary(model.price1)
model.price2 <- lm(price ~ speed+hd+ram+screen+ads+trend, data = Computer_Data)
summary(model.price2)
model.price3 <- lm(price~speed)
summary(model.price3)
# prediction model
pred_price <- predict(model.price, interval = "predict")
pred_price<-as.data.frame(pred_price)
pred_price

