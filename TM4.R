dados<- data.frame(
  indiv = 1:6,
  X1_idade= c(25,40,30,35,28,38),
  X2_renda= c(2000,6000,3500,5000,3000,4500),
  moradia = c("Apartamento", "Casa", "Apartamento", "Casa", "Apartamento",
               "Casa")
)

novos<- data.frame(
  nome= c("João", "José"),
  idade=c(26,40),
  renda=c(2300,6000)

)

moradia<- function(novos, dados, k){
  
  dist<- sqrt((dados$X1_idade - novos$idade)^2 + 
                (dados$X2_renda - novos$renda)^2)
  
  proximos<- dados$moradia[order(dist)[1:k]]
  
  return(names(sort(table(proximos), decreasing = TRUE)[1]))
}

jao1<- moradia(novos[1, ], dados, k = 1)
jao2<- moradia(novos[1, ], dados, k = 3)

ze1<- moradia(novos[2, ], dados, k = 1)
ze2<- moradia(novos[2, ], dados, k = 3)

cat("Tipo de moradia para João \nPelo Critério 1 : ",jao1,
    "\nPelo Critério 2: ",jao2 )

cat("Tipo de moradia para José \nPelo Critério 1: ", ze1, 
    "\nPelo Critério 2: ", ze2)

