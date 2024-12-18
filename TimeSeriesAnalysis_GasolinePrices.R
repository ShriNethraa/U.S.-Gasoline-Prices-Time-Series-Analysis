
library(tidyverse)
library(lubridate)
library(forecast)    
library(tseries)     
library(ggplot2)
library(readr)
gas_price <- read_csv("Downloads/All.U.S.Gasoline_prices_dataset.csv")
gas_price$Date <- parse_date_time(gas_price$Date, orders = c("my", "ym"))

#Converting the Data to Time Series Object
gas_price_ts <- ts(gas_price$`U.S. All Grades All Formulations Retail Gasoline Prices (Dollars per Gallon)`, 
                   start = c(1993, 4), frequency = 12)
plot(gas_price_ts,
     main = "U.S. Gasoline Prices (1993-2024)", 
     xlab = "Year", 
     ylab = "Price (Dollars per Gallon)", 
     col = "brown", 
     lwd = 2)

#Seasonality and Trend Decomposition
decomp_gas_price <- decompose(gas_price_ts)
plot(decomp_gas_price)

#Checking for Stationarity
adf.test(gas_price_ts)

#Differencing the Series
gas_price_diff <- diff(gas_price_ts)
adf.test(gas_price_diff)
plot(gas_price_diff, main = "Differenced U.S. Gasoline Prices")

#Checking Autocorrelations
acf(gas_price_ts, main = "ACF of U.S. Gasoline Prices")
pacf(gas_price_ts, main = "PACF of U.S. Gasoline Prices")

#Fitting an ARIMA Model
arima_model <- auto.arima(gas_price_ts)

summary(arima_model)

checkresiduals(arima_model)

#Forecasting Future Prices
forecasted_values <- forecast(arima_model, h = 12)

plot(forecasted_values, 
     main = "Forecasted U.S. Gasoline Prices (Next 12 Months)",
     xlab = "Year", 
     ylab = "Price (Dollars per Gallon)")

legend("topleft", 
       legend = c("Historical Data", "Forecasted Data", "80% Prediction Interval", "95% Prediction Interval"),
       col = c("black", "dodgerblue", "darkblue", "grey"),
       lty = c(1, 1, 1, 1),
       lwd = c(2, 2, 2, 2),
       bty = "n")

#Evaluating Model Accuracy
accuracy(forecasted_values)

