dados <- read.table("http://www.cnachtsheim-text.csom.umn.edu/Kutner/Chapter%20%201%20Data%20Sets/CH01PR19.txt",
                    header = TRUE)

linha<- data.frame(X1=3.897, X2=21)
dados<- rbind(linha, dados)

media1<- sum(dados$X1)/length(dados$X1)
media2<- sum(dados$X2)/length(dados$X2)

d1<- dados$X1-media1
d2<- dados$X2-media2

d1

norma_d1<- sqrt(sum(d1^2))
norma_d2<- sqrt(sum(d2^2))

norma_d1
norma_d2

prod<- sum(d1*d2)
prod

cos<- prod/(norma_d1*norma_d2)
cos
