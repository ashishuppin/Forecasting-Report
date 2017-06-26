library(readr)
library(forecast)
library(tseries)

Final_Travel <- read_csv("E:/Masters/Sem2/Business Forecasting/EndTerm/Final_Travel.csv")
travel <- Final_Travel$Value
plot(travel)
travel_ts <- ts(travel,start=c(1999,1),frequency = 12)
plot(travel_ts)

updated_travel_ts= window(travel_ts, start=2002) 
plot(updated_travel_ts)
summary(updated_travel_ts)
par(mfrow=c(1,1))
boxplot(updated_travel_ts,horizontal=TRUE)
hist(updated_travel_ts)

decompose_travel <- decompose(updated_travel_ts)
plot(decompose_travel)
print(decompose_travel$type)

stl_decomp <- stl(updated_travel_ts,s.window=5)
plot(stl_decomp)
summary(stl_decomp)
attributes(stl_decomp)

seaAdjusted <-seasadj(stl_decomp)
print(seaAdjusted)

plot(updated_travel_ts, col="grey",
     main="US Toursim", xlab="Year", ylab="No. of Tourists")
lines(seaAdjusted,col="red",ylab="Seasonally adjusted")


############################################################
naive_forecast <- naive(updated_travel_ts,12)
plot(naive_forecast)

plot(naive_forecast$residuals)
hist(naive_forecast$residuals, breaks=10, col="red" ,ylim=c(0,20))


shapiro.test(naive_forecast$residuals)

plot(as.matrix(naive_forecast$fitted), as.matrix(naive_forecast$residuals),main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
plot(as.matrix(updated_travel_ts), as.matrix(naive_forecast$residuals),main="Residuals vs Actual", xlab = "Actual", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0

Acf(naive_forecast$residuals)
Box.test(naive_forecast$residuals, lag=20, type="Ljung-Box")

accuracy(naive_forecast)

print(naive_forecast)
################################################
snaive_forecast <- snaive(updated_travel_ts,12)
plot(snaive_forecast)

plot(snaive_forecast$residuals)
hist(snaive_forecast$residuals, breaks=10, col="red" ,ylim=c(0,20))

shapiro.test(snaive_forecast$residuals)

plot(as.matrix(snaive_forecast$fitted), as.matrix(snaive_forecast$residuals),main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
plot(as.matrix(updated_travel_ts), as.matrix(snaive_forecast$residuals),main="Residuals vs Actual", xlab = "Actual", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0

Acf(snaive_forecast$residuals)
Box.test(snaive_forecast$residuals, lag=20, type="Ljung-Box")

accuracy(snaive_forecast)

print(snaive_forecast)
############################################################
plot(updated_travel_ts, col="grey",
     main="US Toursim", xlab="Year", ylab="No. of Tourists")
lines(ma(updated_travel_ts,3),col="red")
lines(ma(updated_travel_ts,6),col="blue")
lines(ma(updated_travel_ts,12),col="green")

MA3model = forecast(ma(updated_travel_ts,3),h=12)
Acf(MA3model$residuals)
MA6model = forecast(ma(updated_travel_ts,6),h=12)
Acf(MA6model$residuals)

par(mfrow=c(1,1))
Acf(MA3model$residuals)
Acf(MA6model$residuals)

shapiro.test(MA3model$residuals)
shapiro.test(MA6model$residuals)

plot(forecast(ma(updated_travel_ts,3),h=12))
accuracy(forecast(ma(updated_travel_ts,3),h=12))
summary(forecast(ma(updated_travel_ts,3),h=12))
print(MA3model)
############################################################

sse_forecast <- forecast(updated_travel_ts,h=12)
plot(sse_forecast,main="SSE Forecasting Method")
summary(sse_forecast)

plot(sse_forecast$residuals)
hist(sse_forecast$residuals, breaks=10, col="red" ,ylim=c(0,20))

shapiro.test(sse_forecast$residuals)

plot(as.matrix(sse_forecast$fitted), as.matrix(sse_forecast$residuals),main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
plot(as.matrix(updated_travel_ts), as.matrix(sse_forecast$residuals),main="Residuals vs Actual", xlab = "Actual", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0

Acf(sse_forecast$residuals)
Box.test(sse_forecast$residuals, lag=20, type="Ljung-Box")

accuracy(sse_forecast)

print(sse_forecast)

#########################################################

holtwinter_travel <- HoltWinters(updated_travel_ts)
plot(holtwinter_travel,main="Holt-Winter Model")
holtwinter_forecast <- forecast(holtwinter_travel,h=12)
plot(holtwinter_forecast, main="Holt-Winter Forecasting Model")

print(holtwinter_travel)

plot(holtwinter_forecast$residuals)
hist(holtwinter_forecast$residuals, breaks=10, col="red")
shapiro.test(holtwinter_forecast$residuals)

plot(as.matrix(holtwinter_forecast$fitted), as.matrix(holtwinter_forecast$residuals),main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
plot(as.matrix(updated_travel_ts), as.matrix(holtwinter_forecast$residuals),main="Residuals vs Actual", xlab = "Actual", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0

Acf(holtwinter_forecast$residuals)
Box.test(holtwinter_forecast$residuals, lag=20, type="Ljung-Box")

accuracy(holtwinter_forecast)

print(holtwinter_forecast)
###########################################
plot(updated_travel_ts)

# There are test to tell you if series is stationary
# Kipps test says differences is required if p-value is < 0.05
kpss.test(updated_travel_ts)

# NSDIFFS only works for seasonal data and tells us how much difference we need to do
nsdiffs(updated_travel_ts)

updated_travel_ts_diff1 <- diff(updated_travel_ts, differences=1)
ndiffs(updated_travel_ts_diff1)
kpss.test(updated_travel_ts_diff1)

plot(updated_travel_ts_diff1)

Acf(updated_travel_ts_diff1, lag.max=20)

Pacf(updated_travel_ts_diff1, lag.max=20)

fit <- Arima(updated_travel_ts, order=c(0,1,1), seasonal=c(0,1,1))
tsdisplay(residuals(fit))

par(mfrow=c(1,2))

Acf(residuals(fit))
Pacf(residuals(fit))

Option1_ARIMA_travel <- Arima(updated_travel_ts, order=c(1,1,2), seasonal=c(2,1,1))

par(mfrow=c(1,2))

Acf(residuals(Option1_ARIMA_travel))
Pacf(residuals(Option1_ARIMA_travel))

Box.test(residuals(Option1_ARIMA_travel), lag=16, fitdf=4, type="Ljung")



Best_ARIMA_travel <- Arima(updated_travel_ts, order=c(2,1,2), seasonal=c(2,1,0))

par(mfrow=c(1,1))

Acf(residuals(Best_ARIMA_travel))
Pacf(residuals(Best_ARIMA_travel))

Box.test(residuals(Best_ARIMA_travel), lag=16, fitdf=4, type="Ljung")

print(Best_ARIMA_travel)

#AR_S_travel <- Arima(updated_travel_ts_diff1, order=c(1,0,2), seasonal=c(2,1,1)) #this is the one!!
#tsdisplay(residuals(AR_S_travel))
#plot(forecast.Arima(AR_S_travel,h=12))
#plot(forecast.Arima(AR_travel,h=24))



AutoArimaModel <- auto.arima(updated_travel_ts, stepwise=FALSE, approximation=FALSE)

print(AutoArimaModel)
#the p-value is less than 0.05, this we can accept the null hypothesis and state that the timeseries is stationary
#fit <- auto.arima(updated_travel_ts_diff1,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc')

Acf(AutoArimaModel$residuals)
#we can see that no lag is out of bounds

Pacf(AutoArimaModel$residuals)
#the Pacf shows lag 1 and 2 going out of bounds.

Box.test(residuals(AutoArimaModel), lag=16, fitdf=4, type="Ljung")
#AR_S_travel
#plot(fitted(abc), abc$residuals)
#plot(fitted(AR_S_travel), AR_S_travel$residuals)
ARIMA_Forecast <- forecast(Best_ARIMA_travel, h=12)
plot(ARIMA_Forecast, main="Forecast for next 1 year")
plot(forecast(Best_ARIMA_travel, h=24), main="Forecast for next 2 year")

plot(ARIMA_Forecast$residuals)
hist(ARIMA_Forecast$residuals, breaks=10, col="red")
shapiro.test(ARIMA_Forecast$residuals)

plot(as.matrix(ARIMA_Forecast$fitted), as.matrix(ARIMA_Forecast$residuals),main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
plot(as.matrix(updated_travel_ts), as.matrix(ARIMA_Forecast$residuals),main="Residuals vs Actual", xlab = "Actual", ylab = "Residuals") # plot of fitted values vs residuals
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0

Acf(ARIMA_Forecast$residuals)
Box.test(ARIMA_Forecast$residuals, lag=20, type="Ljung-Box")

accuracy(ARIMA_Forecast)

print(ARIMA_Forecast)




