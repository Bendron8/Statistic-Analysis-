dados <- read.table("http://www.cnachtsheim-text.csom.umn.edu/Kutner/Chapter%20%201%20Data%20Sets/CH01PR19.txt",
                    header = TRUE)
colnames(dados)<- c("X1", "X2")

linha<- data.frame(X1 = 3.897, X2 = 21)
names(linha)<- names(dados)
dados<- rbind(linha, dados)

media1<- sum(dados$X1)/length(dados$X1)
media2<- sum(dados$X2)/length(dados$X2)

d1<- dados$X1-media1
d2<- dados$X2-media2

d1
d2

norma_d1<- sqrt(sum(d1^2))
norma_d2<- sqrt(sum(d2^2))

norma_d1
norma_d2

prod<- sum(d1*d2)
prod

cos<- prod/(norma_d1*norma_d2)
cos

cor(dados$X1, dados$X2)

ggplot(dados, aes(x=X1, y=X2)) +
  geom_point(color="green", size=3) +
  labs(title = "Gráfico de Dispersão", x="Pontuação no Vestibular",
       y="Média das Notas") +
  theme_minimal()

model<- lm(X1~X2, data = dados)
coeficiente<- coef(model)
b0<- coef(model)[1]
b1<- coef(model)[2]
cat("Intercepto: ",b0,"\n", "Inclinação: ", b1 )
model
n<- length(dados$X3.897)
vetor_uns<- rep(1,n)

pred<- b0*vetor_uns + b1*dados$X3.897
comparacao<- data.frame(
  obs = dados$X21,
  predito= pred
)
head(comparacao)
