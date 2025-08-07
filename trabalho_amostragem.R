dados<- datasets::iris

boxplot(dados$Petal.Width~dados$Species,
        main="Gráfico da Largura das Pétalas por Espécie",
        xlab = "Espécies",
        ylab = "Largura das Pétalas",
        col = c("lightblue", "lightgreen", "lightpink"))

'''estrat<- split(dados, dados$Species)
amostra<- lapply(estrat, function(x){
  x[sample(nrow(x), size=ceiling(0.2*nrow(x))),]
})

Y<- do.call(rbind, amostra)'''

stratified_sample <- dados %>%
  group_by(dados$Species) %>%  
  sample_frac(0.25) %>%   
  ungroup()              


print(stratified_sample)


table(stratified_sample$Species)

#-------------------------------------------------------

tamanho <- 12  


setosa <- iris %>%
  filter(Species == "setosa") %>%
  sample_n(tamanho)

versicolor <- iris %>%
  filter(Species == "versicolor") %>%
  sample_n(tamanho)

virginica <- iris %>%
  filter(Species == "virginica") %>%
  sample_n(tamanho)

# Calcular a média para cada amostra
media_setosa <- setosa %>%
  summarise(
    Media_Largura_Petala = mean(Petal.Width),
    Media_Comprimento_Petala = mean(Petal.Length)
  )

media_versicolor <- versicolor %>%
  summarise(
    Media_Largura_Petala = mean(Petal.Width),
    Media_Comprimento_Petala = mean(Petal.Length)
  )

media_virginica <- virginica %>%
  summarise(
    Media_Largura_Petala = mean(Petal.Width),
    Media_Comprimento_Petala = mean(Petal.Length)
  )

print("Média para Setosa:")
print(media_setosa)

print("Média para Versicolor:")
print(media_versicolor)

print("Média para Virginica:")
print(media_virginica)

set.seed(123)

#AE
estrat<- iris %>%
  group_by(Species) %>%
  sample_n(tamanho) %>%
  ungroup()

medias_estrat<- estrat %>%
  group_by(Species) %>%
  summarise(
    media_comp = mean(Petal.Length, na.rm = TRUE)
  )
print(medias_estrat)


pesos <- table(iris$Species) / nrow(iris)
media_geral_comprimento <- sum(medias_estrat$media_comp * pesos)
print(media_geral_comprimento)

var_estrat<- estrat %>%
  group_by(Species) %>%
  summarise(
    var_comp = var(Petal.Length, na.rm = TRUE)
  )
print(var_estrat)

pesos2<- table(iris$Species)/nrow(iris)
var_geral_comp<- sum(var_estrat$var_comp*pesos2^2)
print(var_geral_comp)

#AASs
Setosa <- iris[iris$Species == "setosa", ]
Versicolor <- iris[iris$Species == "versicolor", ]
Virginica <- iris[iris$Species == "virginica", ]

x1<-sample(Setosa$Petal.Width, size=12)
x2<-sample(Versicolor$Petal.Width, size=12)
x3<-sample(Virginica$Petal.Width, size=12)


var_setosa <- var(Setosa$Petal.Length, na.rm = TRUE)
var_versicolor <- var(Versicolor$Petal.Length, na.rm = TRUE)
var_virginica <- var(Virginica$Petal.Length, na.rm = TRUE)

print("Variâncias: ")
print(c(
  setosa = var_setosa,
  versicolor = var_versicolor,
  virginica = var_virginica
))

var_geral <- mean(c(var_setosa, var_versicolor, var_virginica))
print(var_geral)


EPA <- var_geral_comp/var_geral
EPA
