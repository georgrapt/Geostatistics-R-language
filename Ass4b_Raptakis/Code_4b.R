
rm(list=ls())
setwd("C:/Users/Giorgos/Desktop/TU Berlin/2nd semester/Geostatistics SS16/homework 4b")
library(rgdal)
library(maptools)
require(lattice)
require(sp)
require(gstat)

# Load up the data
data= read.table("C:/Users/Giorgos/Desktop/TU Berlin/2nd semester/Geostatistics SS16/homework 4b/points.txt",header = TRUE,dec=",")
head(data) #head(), tail() Return the First or Last Part of an Object
fix(data)   #Fix an Object
attach(data)
coordinates(data) <- ~x+y
class(data)

x<-data$X
y<-data$Y
U<-data$V

# A)
plot(x,y,axes = TRUE,xlab = "Xlab", ylab = "Ylab",col =3 ,pch=15)
points(180,120, col='red',pch=20)
legend('topright',inset=.05,c("sample", "estimation"), col = c(3, 'red'), pch=c(15,20))

#B)
Ch <- function(h) { ## This function returns the covariance for given distance h
  c<-2000*exp(-(h)/250)
  }

#C
plot(Ch, xlim=c(0,1800), type='l', col = 45, xlab = "Distance/Lag",
     ylab = "Covariance-Variogram")
variogram = function(h){Ch(0)-Ch(h)}
plot(variogram, xlim=c(0,1800), type='l', col = 'blue',add = TRUE)
legend("center", inset=.05, bty="n", cex=1,title="Covariance - variogram",
       c("Covariance", "Variogram"),fill=c(45,'blue'))


Dist = matrix(NA,nrow=4 ,ncol=4)
for( i in 1: 4 ) {
  for( j in 1:4 ) {
    Dist[i,j] = ( (data[i,1]-data[j,1])^2.0 + (data[i,2]-data[j,2])^2.0 )^0.5
  } 
}
C = matrix(1,nrow=5,ncol=5)
C[1:4,1:4] = Ch(Dist)
C[5,5] = 0

#A Covariance matrix is built showing the dispersion of variables 
#around the mean. The more the dispersion is around mean then the 
#more they vary.

#D
x<-print(round(C,2))

#E
require(MASS)
Ci=ginv(C) ##inverse covariance matrix 
I=Ci%*%C
#Inverse matrix is a measure of how tightly clustered the variables 
#are around the mean (the diagonal elements) and the extent to which 
#they do not co-vary with the other variables (the off-diagonal 
#elements). Thus, the higher the diagonal element, the tighter the 
#variable is clustered around the mean. The interpretation of the 
#off-diagonal elements is more subtle and I refer you to the other 
#answers for that interpretation.'


#f
DI<- c(NA[1:4])
for( i in 1:4 ) {
  DI[i] = sqrt((data[i,1]-180)^2.0 + (data[i,2]-120)^2.0 )
}

D = Ch(DI) 
D[5] = 1

#h
w<-Ci %*% D ##expected value for 
sum(w[1]+w[2]+w[3]+w[4])

#i
EsP<-0
for( i in 1:4 ){
  EsP[i] = ( (w[i]*U[i]))
  }
sum(EsP)

#j

c0<-Ch(0)
S<- c(NA[1:4])
for( i in 1:4 ){
  S[i] =(w[i]*D[i]-w[5])
 }
S
sum(S)
óu<- c0-sum(S)
óu

D
w[1]*D[1]
w[2]*D[2]
w[3]*D[3]
w[4]*D[4]
