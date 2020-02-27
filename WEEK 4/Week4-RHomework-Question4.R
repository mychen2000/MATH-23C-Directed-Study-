rm(list = ls())

#Part a:
#Check that gamma(3/4) * gamma(5/4) = (sqrt(2)*pi)/4
equal <- (gamma(3/4)*gamma(5/4)) == ((sqrt(2)*pi)/4); equal
#it is an true argument

#Part b:
f1 <- function(x){
  x^(3/4 - 1)*exp(-x)
}
f2 <- function(x){
  x^(5/4 - 1)*exp(-x)
}
x <- integrate(f1, 0, Inf); x; gamma(3/4)
y <- integrate(f2, 0, Inf); y; gamma(5/4)
round(x$value*y$value-(sqrt(2)*pi)/4)
# it is 0 for this question

#Part c:
#from Dr. Bamberg's note
rmc <- function(f, a, b, N){
  ff <- function(x) f(x)*((x>=a)&(x<=b))
  aInt <- floor(a); bInt <- ceiling(b)
  Rsum <- 0
  for(i in 1:((bInt - aInt)*2^N)){
    xEval <- runif(1,min=aInt, max=bInt)
    Rsum <- Rsum + ff(xEval)/(2^N)
  }
  return(Rsum)
}
b<- 10^4
N<- 5
X<- rmc(f1, 0, b, N); X; gamma(3/4)
Y<- rmc(f2, 0, b, N); Y; gamma(5/4)

round(X*Y-sqrt(2*pi)/4)
#It is 0