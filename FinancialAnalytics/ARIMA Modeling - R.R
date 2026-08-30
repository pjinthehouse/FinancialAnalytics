## Stock Price Prediction using ARIMA ##

# Activate the packages required
library(quantmod)    ## if it is not installed, run "install.packages("quantmod")
library(tseries)
library(timeSeries)
library(forecast)

# Import the stock price historical data
getSymbols("SUZLON.NS", from = "2015-01-01", to = "2023-09-30")
stockprices = SUZLON.NS[,6]
stockprices = na.omit(stockprices)
return_index <- na.omit(100*diff(log(stockprices)))     #### compute the log returns and store
return_index_sq <- return_index^2

# Diagnostic Test - 1: Test for presence of autocorrelation
## we can use ACF and PACF plots to test
par(mfrow=c(1,2))
Acf(return_index)  # autocorrelation plot
pacf(return_index)   # partial autocorrelation plot
## alternatively we can use Ljung-Box test to test for autocorrelation
# Ho: THERE IS NO AUTOCORRELATION IN THE RETURN SQUARED 
# if p-value is less than 0.05, reject the null
Box.test(coredata(stockprices), type = "Ljung-Box", lag = 10)

# Diagnostic Test - 2: Test for presence of stationarity
## we can use Augmented Dickey-Fuller Test (ADF test)
# Ho: THE DATA IS NON-STATIONARY
# if p-value is less than 0.05, reject the null
adf.test(return_index)

## Divide the data into training and test set
return_training = return_index[1:(0.9*length(return_index))]
return_test = return_index[(0.9*length(return_index)+1):length(return_index)]

## Determine the right ARIMA model to forecast
auto.arima(return_training, ic = "aic", seasonal = FALSE, allowdrift = FALSE, trace = TRUE)

## Create the model
model = Arima(return_training, order = c(0,0,1))
model

## make predictions
preds = predict(model, n.ahead = length(return_index-(0.9*length(return_index))))$pred
preds

## test for accuracy
accuracy(preds, return_test)




















