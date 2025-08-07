dados<- read.csv("C:/Users/brendon/Downloads/bcdata.sgs.22701.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)

dados$data <- as.Date(dados$data, format = "%d/%m/%Y")

dados$valor <- as.numeric(gsub(",", ".", dados$valor))

library(ggplot2)

ggplot(dados, aes(x = data, y = valor)) +
  geom_line(color = "steelblue") +
  labs(title = "Balanço de Pagamentos - Brasil",
       x = "Data", y = "Valor (US$ milhões)") +
  theme_minimal()

library(forecast)

serie_ts <- ts(dados$valor, start = c(1995, 1), frequency = 12)

decomposicao <- decompose(serie_ts)
plot(decomposicao, main = " ")

bp<-diff(serie_ts)
plot(bp)

acf(serie_ts, main = "Balanço de Pagamentos")
pacf(serie_ts, main = "Balanço de Pagamentos")

library(tseries)
adf.test(serie_ts)

serie_dif<- diff(serie_ts, differences = 2)
plot(serie_dif, main = "Série Temporal Diferenciada", ylab = "Valor", xlab = "Tempo")
serie_dif

adf.test(serie_dif)

acf(serie_dif, main = "ACF - Série Diferenciada")
pacf(serie_dif, main = "PACF - Série Diferenciada")


modelo <- arima(serie_ts, order = c(2, 2, 0))

summary(modelo2)

modelo1 <- arima(serie_ts, order = c(1, 1, 1),
                      seasonal = list(order = c(2, 0, 0), period = 12))

modelo2 <- Arima(serie_ts, order = c(1, 1, 1),
                 seasonal = list(order = c(1, 1, 0), period = 12))

modelo3 <- arima(serie_ts, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 12))

modelo4 <- arima(serie_ts, order = c(1,1,2), seasonal = c(1,0,1))


acf(residuals(modelo1))
pacf(residuals(modelo))
hist(residuals(modelo1))
Box.test(residuals(modelo2), lag = 10, type = "Ljung-Box")

checkresiduals(modelo4)

cat("AIC modelo1:", AIC(modelo1), "\n")
cat("AIC modelo2:", AIC(modelo2), "\n")

Box.test(residuals(modelo4), lag = 4, type = "Ljung-Box", fitdf = 3)
Box.test(residuals(modelo2), lag = 24, type = "Ljung-Box", fitdf = 3)
Box.test(residuals(modelo2), lag = 36, type = "Ljung-Box", fitdf = 3)


previsao <- forecast::forecast(modelo2, h = 12)

# Plotar previsões
autoplot(previsao) +
  ggtitle("Previsões do Modelo para o Balanço de Pagamentos") +
  xlab("Ano") +
  ylab("Valor (US$ milhões)") +
  theme_minimal() 
