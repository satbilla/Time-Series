rm(list=ls(all=TRUE))


library(forecast)
library(quantmod)
library(ggplot2)

# Read file ####
setwd("D:/Data Science/INSOFE/CuTe/time")
data_Daily <- read.csv("TS_Train.csv")
head(data_Daily)

# Basic Visualization 


plot(data_Daily$value, type = "l", col = 'blue', xlab = 'timestamp', ylab = 'Value',
     main = 'Basic Visualization')
axis(1, at = seq(0, 90, by = 15))

# Dividing data as Train & validation ####
train=data_Daily[which(data_Daily$timestamp <= 72),]
validation=data_Daily[which(data_Daily$timestamp>72),]

#Verification 
head(train)
head(validation)
dim(train)
dim(validation)

# train validate #####

# rm(list = setdiff(ls(),c("train","validation")))
Price <- ts(train$value, frequency = 15)
plot(Price,type="l", lwd=3, col="blue", xlab="7 days", ylab="Price", main="Time series plot")
pricedecomp <- decompose(Price)
plot(pricedecomp)



# Plot ACF and PACF to get preliminary understanding of the process
par(mfrow = c(1, 2))
acf(Price)
pacf(Price)


# The suspension bridge pattern in ACF suggests both nonstationarity
# and strong seasonality.  Perform a non-seasonal difference to give an ARIMA(0,1,0) model
par(mfrow = c(1, 1))
Pricediff1 <- diff(Price, differences = 1)
plot(Pricediff1)

# Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(Pricediff1)
pacf(Pricediff1)


# The differenced series looks stationary but looks like seasonal lags
# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)15)
par(mfrow = c(1, 1))
priceseasonaldiff1 <- diff(Price, lag = 7, differences=1)
plot(priceseasonaldiff1)


# Check ACF and PACF for seasonally differenced data
# to explore remaining dependencies
par(mfrow = c(1, 2))
acf(priceseasonaldiff1)
pacf(priceseasonaldiff1)
# Strong positive autocorrelation indicates need for either an AR component
# or a non-seasonal differencing.  Perform AR component.


#ARIMA(010)
priceArima1 <- Arima(Price, order = c(0,1,0),
                     include.drift = FALSE)
pred_train = fitted(priceArima1)
pred_validation <- data.frame(forecast(priceArima1,h = 18))$Point.Forecast
DMwR::regr.eval(train$value, pred_train )
DMwR::regr.eval(validation$value, pred_validation )
ARIMA010 <- DMwR::regr.eval(validation$value, pred_validation )
# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(priceArima1$residuals, lag.max = 15)
pacf(priceArima1$residuals, lag.max = 15)
Box.test(priceArima1$residuals, lag=15, type="Ljung-Box")
# Start forecasting
library(forecast)
par(mfrow = c(1, 1))
priceforecastsArima1 <- forecast(priceArima1, h=18)
plot(forecast(priceforecastsArima1))
priceforecastsArima1



#ARIMA(010)(100)
priceArima1 <- Arima(Price, order = c(0, 1, 0), seasonal = c(1, 0, 0),
                     include.drift = FALSE)
pred_train = fitted(priceArima1)
pred_validation <- data.frame(forecast(priceArima1,h = 18))$Point.Forecast
DMwR::regr.eval(train$value, pred_train )
DMwR::regr.eval(validation$value, pred_validation )
ARIMA010100 <- DMwR::regr.eval(validation$value, pred_validation )
# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(priceArima1$residuals, lag.max = 15)
pacf(priceArima1$residuals, lag.max = 15)
Box.test(priceArima1$residuals, lag=15, type="Ljung-Box")
# Start forecasting
library(forecast)
par(mfrow = c(1, 1))
priceforecastsArima1 <- forecast(priceArima1, h=18)
plot(forecast(priceforecastsArima1))
priceforecastsArima1



#ARIMA(010)(100)
priceArima1 <- Arima(Price, order = c(0, 1, 0), seasonal = c(1, 0, 0),
                     include.drift = TRUE)
pred_train = fitted(priceArima1)
pred_validation <- data.frame(forecast(priceArima1,h = 18))$Point.Forecast
DMwR::regr.eval(train$value, pred_train )
DMwR::regr.eval(validation$value, pred_validation )
ARIMA010100TRUE <- DMwR::regr.eval(validation$value, pred_validation )
# Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(priceArima1$residuals, lag.max = 15)
pacf(priceArima1$residuals, lag.max = 15)
Box.test(priceArima1$residuals, lag=15, type="Ljung-Box")
# Start forecasting
library(forecast)
par(mfrow = c(1, 1))
priceforecastsArima1 <- forecast(priceArima1, h=18)
plot(forecast(priceforecastsArima1))
priceforecastsArima1



# Auto ARIMA ####

priceAutoArima <- auto.arima(Price,ic='aic')
priceAutoArima
pricetimeseriesforecastsAutoArima <- forecast(Price, h=18)
plot(forecast(pricetimeseriesforecastsAutoArima))
pred_train = fitted(priceAutoArima)
pred_validation = data.frame(forecast(priceAutoArima, h = 18))$Point.Forecast
DMwR::regr.eval(train$value, pred_train )
DMwR::regr.eval(validation$value, pred_validation )
AutoARIMA <- DMwR::regr.eval(validation$value, pred_validation )


### final 15010100 ####
Price <- ts(data_Daily$value, frequency = 15)

priceArima1 <- Arima(Price, order = c(0,1,0),
                     seasonal = c(1,0,0), include.drift = FALSE)

par(mfrow = c(1, 1))
priceforecastsArima1 <- forecast(priceArima1, 
                                 h=7)
plot(forecast(priceforecastsArima1))
data.frame(priceforecastsArima1)$Point.Forecast

result <- data.frame(timestamp = c(91:97), value = data.frame(priceforecastsArima1)$Point.Forecast)
write.csv(result, 'TS_Submission15010100.csv', row.names = FALSE)



### final 15010100 true ####
Price <- ts(data_Daily$value, frequency = 15)

priceArima1 <- Arima(Price, order = c(0,1,0),
                     seasonal = c(1,0,0), include.drift = TRUE)

par(mfrow = c(1, 1))
priceforecastsArima1 <- forecast(priceArima1, 
                                 h=7)
plot(forecast(priceforecastsArima1))
data.frame(priceforecastsArima1)$Point.Forecast

result <- data.frame(timestamp = c(91:97), value = data.frame(priceforecastsArima1)$Point.Forecast)
write.csv(result, 'TS_Submission15010100TRUE.csv', row.names = FALSE)


### Smoothing models
plot(Price,type="l",lwd=3,col="blue",xlab="week",ylab="Price", main="Time series plot")

# install.packages('TTR')
library(TTR)
fitsma <- SMA(Price,n=15)
length(fitsma)

par(mfrow=c(2,2))

View(data.frame(train$value, SMA(Price,n=90)))
plot(train$value, type="l", col="black")
plot(SMA(Price,n=15), type="l", col="red")
plot(WMA(Price,n=15), type="l", col="blue")
plot(EMA(Price,n=15), type="l", col="brown")

par(mfrow=c(1,1))
plot(train$value, type="l", col="black", main = 'Smoothing Models')
lines(SMA(Price,n=15), col="red")
lines(WMA(Price,n=15), col="blue")
lines(EMA(Price,n=15), col="brown")
legend(50, 2.7, legend=c("SMA", "WMA", "EMA"),
       col=c("red", "blue", "brown"), lty=1:2, cex=0.8)



# ### Holt winter's Model####
# #Building the Holt winter's model taking only Trend component. 
Price <- ts(train$value, frequency = 15)
hw_forecast <-  HoltWinters(Price,  beta=TRUE, gamma=FALSE)
plot(hw_forecast)
hw_forecast$SSE
hw_price_forecasts = forecast(hw_forecast,h = 18)
test_preds <- data.frame(hw_price_forecasts)$Point.Forecast
test_actuals <- validation$value
# install.packages('DMwR')
library(DMwR)
DMwR::regr.eval(test_actuals,test_preds)
HoltWintersTrend <-  DMwR::regr.eval(test_actuals,test_preds)

resids <- test_actuals - test_preds
par(mfrow=(c(1,1)))
pacf(resids)



# Result ###
# Price <- ts(data_Daily$value, frequency = 15)
# hw_forecast <-  HoltWinters(Price,  beta=TRUE, gamma=FALSE)
# hw_price_forecasts = data.frame(forecast(hw_forecast,h = 7))$Point.Forecast
# hw_forecast$SSE
# hw_price_forecasts
# result <- data.frame(timestamp = c(91:97), value =hw_price_forecasts)
# write.csv(result, 'TS_Submission15HoltWinters.csv', row.names = FALSE)


# ### Holt winter's Model Multiplicative####
hw_forecast_Mult <-  HoltWinters(Price, beta=TRUE, gamma=FALSE,
                             seasonal="multiplicative")
plot(hw_forecast_Mult)
hw_price_forecasts = data.frame(forecast(hw_forecast_Mult,h = 18))$Point.Forecast
hw_forecast_Mult$SSE
hw_price_forecasts

test_actuals <- validation$value
HoltWintersMulti <- DMwR::regr.eval(test_actuals,hw_price_forecasts)


###Building the Holt winter's model taking Trend component along with the seasonal component.
library('forecast')
hw_forecast2 <-  HoltWinters(Price, beta=TRUE, gamma=TRUE)
hw_forecast2$SSE
plot(hw_forecast2)
hw_price_forecasts = forecast(hw_forecast2,h = 18)
test_preds <- data.frame(hw_price_forecasts)$Point.Forecast
test_actuals <- validation$value
# install.packages('DMwR')
library(DMwR)
DMwR::regr.eval(test_actuals,test_preds)

HoltWintersTrendSea <-  DMwR::regr.eval(test_actuals,test_preds)
resids <- test_actuals - test_preds
par(mfrow=(c(1,1)))
pacf(resids)

# Errors ####
finalerros <- data.frame(rbind(
  ARIMA010,
  ARIMA010100,
  ARIMA010100TRUE,
  AutoARIMA,
  HoltWintersTrend,
  HoltWintersMulti,
  HoltWintersTrendSea
  ))

finalerros[order(-finalerros$mape),]
