data("co2")

carbon<- ts(co2, start = c(1959,1), frequency = 12)
plot(carbon, 
     main = "Série CO2",
     xlab = "Ano",
     ylab = "Concentração", 
     col = "blue",
     lwd = 2)

time_index<- time(carbon)
linearM<- lm(carbon~time_index)

abline(linearM, col = "red", lwd=2)

resi<- residuals(linearM)
plot(time_index, resi, main = "Resíduos vs. Tempo",
     xlab = "Ano", ylab="Resíduos", col = 'blue', pch = 16)
abline(h=0, col = 'red', lwd = 2)

resid_ts<- ts(resi, start = c(1959,1), frequency = 12)
ts.plot(resid_ts,
        ylab = "Resíduos", 
        xlab = "Ano", 
        col = "blue", 
        lwd = 2)
abline(h = 0, col = "red", lwd = 2)


grafico.sazonalidade<-function(x,s){
  x<-as.vector(x)
  N<-length(x)
  Mx<-max(x)
  mx<-min(x)
  valor<-N%/%s
  eixox<-seq(1:s)
  plot(eixox,x[1:s],ylim=c(mx,Mx),xlim=c(0,(s+1)),type="l",
       xlab = "Mês", ylab = "Concentração de CO2",
       main = "Gráfico de Sazonalidade - CO2", col = "blue")
  for (i in 1:valor){
    y<-rep(NA,s)
    y[1:s]<-x[((i-1)*s+1):(i*s)]
    lines(y,col=i)
  }
  y<-rep(NA,s)
  y<-x[(valor*s+1):N]
  lines(y,col=(valor+1))
}

grafico.sazonalidade(carbon, 12)


boxplot(co2,
        main = "Boxplot das Concentrações de CO2",
        ylab = "Concentração de CO2",
        col = "lightgreen",
        border = "darkgreen")
summary(co2)

diff_co2<- diff(co2)
plot(diff_co2, 
     main = "Gráfico de Diferenças",
     xlab="Tempo",
     ylab = "Diferença",
     col = "blue",
     type = 'l',
     lwd = 2)
abline(h=0, col = "red", lty=2)
