library(forecast)

####################################
# Holt Winters Exponential Smoothing
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirtimeseries)

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)


souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
plot(souvenirtimeseriesforecasts)

# Holt Winters Exponential Smoothing Forecasting
souvenirtimeseriesforecasts2 <- forecast:::forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
plot.forecast(souvenirtimeseriesforecasts2)
plot(souvenirtimeseriesforecasts2)
