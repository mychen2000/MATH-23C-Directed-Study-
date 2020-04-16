rm(list = ls())

#define the function
f0 <- function(x){
  x^0 * exp(-x)
}
f1 <- function(x){
  x^1 * exp(-x)
}
f2 <- function(x){
  x^2 * exp(-x)
}
f3 <- function(x){
  x^3 * exp(-x)
}

v0 <- c(1, 0, 0, 0)
v1 <- c(0, 1, 0, 0)
v2 <- c(0, 0, 1, 0)
v3 <- c(0, 0, 0, 1)

#define a function to evaluate polynomial functions
poly <- function (coeff, x) {
  sum(coeff*c(f0(x), f1(x), f2(x), f3(x)))
}

#inner product function
inner <- function(v, w){
  fprob <- function(x) poly(v, x)*poly(w, x)
  integrate(Vectorize(fprob), 0, Inf)$value
}
n0 <- v0/sqrt(inner(v0,v0)); n0
sqrt(inner(n0,n0))

#Take v2 and substract in the direction of v1
u1 <- v1 - inner(v1, n0)*n0; u1
n1 <- u1/sqrt(inner(u1, u1)); n1
# check for orthogonality
inner(n0,n1)
sqrt(inner(n1,n1))

#repeat
u2 <- v2 - inner(v2, n0)*n0-inner(v2, n1)*n1; u2
n2 <- u2/sqrt(inner(u2, u2)); n2
# check for orthogonality
inner(n0,n2)
inner(n1,n2)
sqrt(inner(n2,n2))

#repeat
u3 <- v3 - inner(v3, n0)*n0-inner(v3, n1)*n1 - inner(v3, n2)*n2; u3
n3 <- u3/sqrt(inner(u3, u3)); n3
# check for orthogonality
inner(n0,n3)
inner(n1,n3)
inner(v2,n3)
sqrt(inner(n3,n3))

#Check that ni are basis
#check linear dependence
install.packages("pracma")
library(pracma)
A <- cbind(n0, n1, n2, n3)
rref(A)#the resulting matrix shows that rows are linearly independent

n0; n1; n2; n3

#abstract basis function
a0 <- function(x) {n0[1]*f0(x) + n0[2]*f1(x) + n0[3]*f2(x) + n0[4]*f3(x)}
a1 <- function(x) {n1[1]*f0(x) + n1[2]*f1(x) + n1[3]*f2(x) + n1[4]*f3(x)}
a2 <- function(x) {n2[1]*f0(x) + n2[2]*f1(x) + n2[3]*f2(x) + n2[4]*f3(x)}
a3 <- function(x) {n3[1]*f0(x) + n3[2]*f1(x) + n3[3]*f2(x) + n3[4]*f3(x)}

curve(a0, from = -1, to = 1, col = "green")
curve(a1, from = -1, to = 1, col = "blue", add=TRUE)
curve(a2, from = -1, to = 1, col = "red", add=TRUE)
curve(a3, from = -1, to = 1, col = "black", add=TRUE)
