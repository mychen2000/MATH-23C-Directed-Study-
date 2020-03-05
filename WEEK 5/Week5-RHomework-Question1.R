rm(list = ls())

#Part a:
f <- function (n){
  delta<-1/2^n
  left <- seq(from = 0, to = 1 - delta, by = delta)
  tbl <- expand.grid(left, left, left)
  #get coordinates of the cubes, (2^n)^3 cubes are there total
  totalCubes <- (2^n)^3
  x<- tbl$Var1 + runif(totalCubes, 0, delta)
  y<- tbl$Var2 + runif(totalCubes, 0, delta)
  z<- tbl$Var3 + runif(totalCubes, 0, delta)
  
  expOur <- mean(z^2)
  expOppo <- mean(x*y)
  
  pOurs <- mean(z^2<x*y)
  
  return(c(expOur, expOppo, pOurs))
}

loop <- replicate(n = 25, f(8)); head(loop)
#getting all possible emans and variances
mean(loop[1,])
var(loop[1,])
mean(loop[2,])
var(loop[2,])
mean(loop[3,])
var(loop[3,])
#all variances are prtty close to zero, 
#and mean very close to the actual valuea

#Part b:
#want to sample from the entire cube
f1 <- function(n){
  totalCubes <- (2^n)^3
  x<- runif(totalCubes, 0, 1)
  y<- runif(totalCubes, 0, 1)
  z<- runif(totalCubes, 0, 1)
  #get things to return
  expOur <- mean(z^2)
  expOppo <- mean(x*y)
  pOurs <- sum(z^2 < x*y)/(totalCubes)
  
  return(c(expOur, expOppo, pOurs))
}

loop1 <- replicate(n=25, f1(8)); head(loop1)
#getting all possible emans and variances of ourScore
mean(loop1[1,])
var(loop1[1,])
mean(loop1[2,])
var(loop1[2,])
mean(loop1[3,])
var(loop1[3,])
#Again, all are close to what is expected.