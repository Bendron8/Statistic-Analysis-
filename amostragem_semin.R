dados<- CO2
View(dados)


agg_data <- aggregate(uptake ~ conc, data = CO2, mean)

serie_temporal <- ts(agg_data$uptake, start = 1, frequency = 1)
plot(serie_temporal, type = "o", col = "blue")

serie_t<- ts(dados$uptake, start= 1, frequency = 1)
ts.plot(serie_t)

length(dados$uptake)
te<- seq(1:84)
cosx<- cos(2*pi*te/12)
sinx<- sin(2*pi*te/12)

mm<- lm(dados$uptake~cosx+sinx)
summary(mm)
ts.plot(mm$residuals)
