y1<- c(10,15,20)
y2<- c(64,63,84)

medias<- mean(y1)
medias2<- mean(y2)

nomes<- c("x_1", "x_2", "x_3")
plot(y1, y2, xlim= c(0,30),
     ylim= c(60,90), pch=19, col="green")

text(y1, y2, labels = nomes, pos = 3,
     offset = 0.4)

points(mean(y1), mean(y2), pch=4, col = 'red', cex=1.5 )

abline(h=medias2, v=medias, col='grey', lty=2)
#--------------------------------------------------------

d1<- c(-5,0,5)
d2<- c(-6.3,-7.3,13.7)

nome<- c("x'_1", "x'_2", "x'_3")
d_<- mean(d1)
d_2<- mean(d2) 

plot(d1, d2, xlim = c(-10,10), ylim=c(-15,15), 
     pch=19, col="green")

points(mean(d1), mean(d2), pch=4, col='red', cex=1.5)
abline(h=d_2, v=d_, col='grey', lty=2)
#-------------------------------------------------------
