rm(list = ls())

#Part a;
#no variance given in the question, have to pick one at random. Inspired in the office hour
sigma <- 2 # set the variance to be 4
mu <- 0
#mean = 0, fourth central moment
f <- function(x){
  (x-mu)^4 * dnorm(x, mean = mu, sd = sigma)
}
#integrate
fcm <- integrate(f, -Inf, Inf); fcm
round(fcm$value/(sigma^4) -3)

#Part b:
#get the second moment
f <- function(x){
  x^2 * dunif(x, min = -1, max = 1)
}
exp2 <- integrate(f, -1, 1); exp2
#mean value
g <- function(x){
  x * dunif(x, min = -1, max = 1)
}
exp <- integrate(g, -1, 1); exp
var <- exp2$value-(exp$value)^2; var
# find 0.333...
#get the fourth
f <- function(x){
  (x-mu)^4*dunif(x, min = -1, max = 1)
}
fcm.uni <- integrate(f, -Inf, Inf); fcm.uni
fcm.uni$value/(var)^2 - 3