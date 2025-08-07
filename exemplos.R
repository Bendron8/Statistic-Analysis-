x = c(2, 3, 4, 5.1, 3.2); x
y <- c(x,0,1,x); y
z <- c(2.4, 1.3, 3.4, 2.1, 5.7)
w <- 2*x+z+1;w
rx <- range(x);rx
lx <- length(x);lx
num1 10
num2=3
resto=num1%%num2; resto

#alternativa
modm= function(num1,num2){
  div=num1/num2
  resto=num1%%num2
  return(list(div=div, resto=resto))
}
modm(10,3)
modm(3,10)
modm(2,4)
#==========================================================
a10= function(numero){
  if (numero<10)
    print("Carro Novo!")
  else
    print("Carro Velho!")
}
a10(5)
#==========================================================
tempo=function(){
  tempo=readline("Quanto tempo tem seu carro: ")
  tempo=as.integer(tempo)
  if (tempo<= 3)
    print('Carro Novo!')
  else
    print ('Carro Velho!')
}
tempo()
#==========================================================
aluno=function(p1,p2,p3,t,L){
  n=(p1+p2+p3+t)/4 + L
  if(n>=10)
    n=10
  else n
  if (n>=6)
    cat(paste('Sua nota média é: ', n, '. Aprovado'))
  else
    cat(paste('Sua nota média é: ', n,'. Reprovado'))
}
aluno(5,3,5,6,1)
aluno(10,10,10,10,1)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nota=c(5,3,5,6)
md= function(nota){
  n=length(nota)
  m=sum(nota)/n + 1
  return(m)
}
md(nota)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
x1<- seq(0,100, by=0.1)
y1<- 5*log(x1)
plot(x1,y1, type = "l", col=2)

y=function(x) 5*log(x)
y1=y(x1)
plot(x1,y1, type = "l", col=2, lwd=2,
     panel.first = grid())

x2<- seq(80,120,l=101)
y2<- (1/sqrt(50*pi))*exp(-0.02*(x2-100)^2)
plot(x2,y2, type="l", col=2, lwd=2, panel.first = grid())
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
x<- seq(1,5, by=1)
y<- seq(1,5, by=1)
f<- function(x,y) x^2+y^2
z<- outer(x,y,f); z

#par(mfrow=c(1,2) #coloca dois graficos 
persp(x,y,z, theta=30, phi = 30, col = "lightblue")
persp(x,y,z, theta=20, phi=30, col='lightblue', ticktype='detailed', 
      nticks=5)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
a<-seq(-1,1, l=50)
b<-seq(-1,1,l=50)
c<- function(a,b) sqrt(a^2+b^2)
d<- outer(a,b,c)
persp(a,b,d, theta=30, phi=30,
      col = 'lightgreen', ticktype = 'detailed', nticks = 5)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
s<- seq(-5,5, l=50)
t<- seq(-5,5, l=50)
w<- function(s,t)
  cos(s)*cos(t)*exp((-sqrt(s^2+t^2))/4)
v<- outer(s,t, w)
persp(s,t,v, theta=30,
      phi=30, col='lightsalmon',
      ticktype='detailed', nticks=5)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
a<-seq(-1,1, l=50)
b<-seq(-1,1,l=50)
c<- function(a,b) sqrt(a^2+b^2)
d<- outer(a,b,c)
library(plotly)
plot_ly(x=a, y=b, z=d, type = 'surface')
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

x <- c(1, 5, 10, 15, 20, 25, 30, 35)
y <- c(8, 16, 14, 21, 38, 25, 43, 39)
plot(x,y,
     col="blue", pch=1, type="p",
     cex=1.0,lty=1,xlim=c(0,45),
     ylim=c(0,50),xlab="vari´avel x",
     ylab="vari´avel y",main = "T´ıtulo")
plot(x,y, type = 'b')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
nome=c("Lisa","Godofredo","João","Joana","Alba")
curso=c("superior","superior","fundamental","médio","médio")
idade=c(34,43,21,37,25)
salario=c(1100,1450,450,960,600)
anos=c(5,8,3,8,2)
sexo=c("F","M","M","F","F")
df=data.frame(nome,curso,idade,salario,anos,sexo,
              stringsAsFactors = FALSE)
df
table(df$nome, df$idade)
prop.table(table(df$salario))
50/200
nclass.Sturges(df$salario)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
base<- data.frame(iris)
table(iris)
head(base)

# Criando uma tabela de contingência de exemplo
tabela <- table(Sexo = c("Masculino", "Feminino", "Masculino", "Feminino"),
                Nota = c("A", "B", "C", "A"))

# Exibindo a tabela de contingência
print(tabela)

# Calculando as proporções
proporcoes <- round(prop.table(tabela)*100,2)
print(proporcoes)
