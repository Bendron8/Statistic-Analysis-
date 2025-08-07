dados<- read_excel("C:/Users/brendon/Downloads/dados lista 5.xls")
antes <- dados$antes
depois<- dados$depois

dif<- c(antes-depois)
dif

boxplot(antes, depois)
qqnorm(dif)

mean(dif)
sd(dif)

t.test(antes, depois, alternative = "two.sided",
       paired= T, conf.level = .95)

#testes de hipóteses para a diferença entre as amostras.(deve ser o mesmo resultado)

t.test(dif, alternative = "two.sided", mu=0.0,
       conf.level = .95)


#lista 6

dados_enem<- read_excel("C:/Users/brendon/Downloads/dados lista 6 nova.xlsx")
plot(dados_enem$enem, dados_enem$univers,
     xlab = "Nota do Enem",
     ylab = "Nota na Universidade")

modelo<- lm(dados_enem$enem~dados_enem$univers, data = dados_enem)
modelo$coefficients
abline(modelo, lwd = 2)


notas<- c(dados_enem$enem, dados_enem$univers)
qqnorm(notas)

menor <- subset(dados_enem<720)
maior <- subset(dados_enem>=720)
plot(, maior,
     xlab = "Nota do Enem", ylab = "Nota Universitária")
