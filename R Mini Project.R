#Checking & Setting Directory, Loading Dataset & Required Libraries.
getwd()
setwd("D:/")
library(prophet)
library(lubridate)
library(ggplot2)
btc <- read.csv('BTC-USD.csv')
head(btc)
tail(btc)
#DATA PRE-PROCCEING
str(btc)
btc$Date <- dmy(btc$Date)
str(btc)
qplot(Date, Close, data=btc, main="Bitcoin Closing Prices")
#Converting Normal Dataset Into Logarithmic Type As Per Requirement of This Algorithm.
ds <- btc$Date
y <- log(btc$Close)
btcdf <- data.frame(ds, y)
head(btcdf)
#Plotting Converted Dataset.
qplot(ds, y, data=btcdf, main = "Bitcoin Closing Prices in log scale")
#Creating The Model.
model <- prophet(btcdf)
str(model)
future <- make_future_dataframe(model, periods = 365)
tail(future)
#Forecasting The Model
forecast <- predict(model, future)
head(forecast)
dyplot.prophet(model, forecast)
#Checking Accuracy of Model
pred <- forecast$yhat[1:2710]
actual <-  model$history$y
plot(actual, pred, pch = c(15),col=c("blue" , "green"))
legend("topleft", legend = c("actual","pred"), fill = c("blue","green"))
abline(lm(pred~actual),col = 'black')
summary(lm(pred~actual))
x <- cross_validation(model, 365, units = 'days')
performance_metrics(x)
plot_cross_validation_metric(x, metric = 'mape')