dados <- read_excel("C:/Users/brendon/Downloads/dados lista 6 nova.xlsx")

plot(dados$enem, dados$univers, xlab = "Nota do Enem",
     ylab = "Nota na Universidade",
     pch = 19)

modelo<- lm(dados$univers ~ dados$enem, data = dados)

abline(modelo)

#_______________________________________________________

menor<- dados[dados$enem < 720,]
maior<- dados[dados$enem >= 720,]

plot(menor$enem, menor$univers,
     main = "Diagrama de dispersão- Nota do Enem < 720 ")

modelo_menor<- lm(menor$univers ~menor$enem, data=dados)
abline(modelo_menor)

plot(maior$enem, maior$univers,
     main = "Diagrama de dispersão - Nota do Enem >= 720")

modelo_maior<- lm(maior$univers~maior$enem, data = dados)
abline(modelo_maior)

#__________________________________________________________

model1 <- lm(menor$univers~menor$enem) 
summary(model1)
anova(model1)

model2<- lm(maior$univers~maior$enem)
summary(model2)
anova(model2)

#___________________________________________________________

resi<- model1$residuals
plot(resi)

resi2<- model2$residuals
plot(resi2)
