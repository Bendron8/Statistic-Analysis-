
ts.plot(nhtemp)


acf(nhtemp)
pacf(nhtemp)

ar<- arima(nhtemp, order = c(2,0,0))
ar

hist(ar$residuals)
ts.plot(ar$residuals)
acf(ar$residuals)
pacf(ar$residuals)

Box.test(ar$residuals, lag = 2, type = "Ljung", fitdf = 1)
