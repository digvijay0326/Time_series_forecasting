######################################
# this data is monthly reatial sales per data
# data adjusted for inflation
# forecasting for next two years 
##########################################

# clear all variables in workspace 
rm(list=ls())

# load the forecasting packages
library(fpp2)

# load the data 
data <- read.csv("real_sales_per_day.csv")

# Declare this as time series data
Y <- ts(data[,2][1:276], start=c(1992, 1), frequency = 12)

#########################################
# prelimary analysis
#########################################

# Time Plot
autoplot(Y) +
  ggtitle("Time Plot: Real US retail Sales Per Day") +
  ylab("Million Dollar as per 2017")

# Data has a strong trend. validating with tests

library(tseries)

# applying Dicky fuller test: Null hypothesis -> series is non stationary
adf_test <- function(Y){
  adf_result <- adf.test(Y)
  p_val_adf <- adf_result$p.value
  if(p_val_adf > 0.05){
    print("Fail to reject null hypothesis: series is non stationary")
  }else{
    print("reject null hypothesis: series is stationary")
  }
}

# applying kpss test: Null hypothesis -> series is stationary
kpss_test <- function(Y){
  kpss_result <- kpss.test(Y)
  p_val_kpss <- kpss_result$p.value
  if(p_val_kpss > 0.05){
    print("Fail to reject null hypothesis: series is stationary")
  } else{
    print("reject null hypothesis: series is non stationary")
  }
}

adf_test(Y)
kpss_test(Y)

# Both test confirms series is non stationary

# Take first difference of data to remove trend

DY <- diff(Y)
# Time plot of Differenced Data

autoplot(DY) +
  ggtitle("Time Plot: Change in Real US retail Sales Per Day") +
  ylab("Million Dollar as per 2017")

# Now series appears trend-stationary, check for seasonality

ggseasonplot(DY) +
  ggtitle("Seasonal Plot: Change in Daily Retail Sales") +
  ylab("Million Dollar as per 2017")

ggsubseriesplot(DY)

##############################################
# Our series Y has trend and seasonality 
# To remove the trend, we take first difference
# The first differenced series still has seasonality

# Forecast with various methods
##############################################

# Use benchmark method to forecast
# Let's use the seasonal naive method as our benchmark
# y_t = y_(t-s) + e_t, s: seasonal period

fit <- snaive(DY) # residual SD = 286.5849
summary(fit)
checkresiduals(fit)

################
# fit ETS method
#################

fit_ets <- ets(Y) # residual SD = 218.8133
summary(fit_ets)
checkresiduals(fit_ets)

###################
# Fit an ARIMA model
###################

fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
summary(fit_arima)
checkresiduals(fit_arima)

########################
# Forecast with the ARIMA Model
########################
fcst <- forecast(fit_arima, h = 36)
# Assuming original_data contains the last three years of your time series data
# Convert it to a ts object (if not already in this format)
original_data_ts <- ts(data[,2][277:312], start = c(2015, 1), frequency = 12)  # Adjust start year as needed
autoplot(fcst, include = 60) +
  autolayer(original_data_ts, series = "Original Data", PI = FALSE) +
  ggtitle("Original vs Forecasted Data (Last Three Years)") +
  xlab("Time") + ylab("Values") +
  scale_color_manual(values = c("Original Data" = "red", "Forecasted Data" = "red")) +
  theme_minimal()
summary(fcst)

