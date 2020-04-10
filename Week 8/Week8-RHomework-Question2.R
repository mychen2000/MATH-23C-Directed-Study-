rm(list = ls())

#Part a:
f0 <- function(x)dt(x, 1)
integrate(f0, -Inf, Inf)$value

f1 <- function(x)x^1*dt(x, 1)
integrate(f1, -Inf, Inf)$value

f2 <- function(x)x^2*dt(x, 1)
integrate(f2, -Inf, Inf)$value
#the result diverges for E(X^2), which means it does not exist

density <- function(x)(1/pi)*(x/(1 +x^2))
integrate(density, -Inf, Inf)$value

density2 <- function(x)(1/pi)*(x^2/(1 +x^2))
integrate(density2, -Inf, Inf)$value
#the probability diverges here.

#Part b:
g1 <- function(x)x^1*dt(x, 1)
integrate(g1, -Inf, Inf)$value
#the result does not match expectation
integrate(g1, -Inf, 0)$value
integrate(g1, 0, Inf)$value
#the result diverges here

N<- 10000
n <- 1000
Xmean <- rep(NA, N)

for (i in 1:N) {
  Xmean[i] <- mean(rt(n, df = 1)) 
}

hist(Xmean, probability = TRUE, col = "blue", breaks = "FD")
#sample mean and expectation do not match as shown in the result
#so R's integration of expecttion is not reliable.

# use large limits:
integrate(g1, -100, 200)$value
integrate(g1, -300, 800)$value
integrate(g1, -1000, 3000)$value
integrate(g1, -5000, 4000)$value

# the eventual result is not 0 in this case.

#Part c:
h1 <- function(x)x^1*dt(x, 2)
integrate(h1, -Inf, Inf)$value
#equal to 0

N<- 10000
n <- 1000
Xmean <- rep(NA, N)

for (i in 1:N) {
  Xmean[i] <- mean(rt(n, df= 2)) 
}
hist(Xmean, probability = TRUE, col = "blue")
#The result converges to 0 and meets the expectation
h2 <- function(x)x^2*dt(x, 2)
integrate(h2, -Inf, Inf)$value
#this integrand diverges here

h3 <- function(x)(x^2)/(2+x^2)^(3/2)
integrate(h3, -Inf, Inf)$value
#still fails to converge. Second moment and variance don't exist

#Part d:
i1 <- function(x)x*dt(x,3)
exp <- integrate(i1, -Inf, Inf)$value; exp
#this tells that the expectation is 0

i2 <- function(x)x^2*dt(x,3)
exp2 <- integrate(i2, -Inf, Inf)$value; exp2

var <- exp2 - exp^2
#the variance here is 3

#Part e:
j1 <- function(x) 1/(4+x^2)^(5/2)
integrate(j1, -Inf, Inf)$value
result <- 1/integrate(j1, -Inf, Inf)$value; result
#so the expected variance is 12

j2 <- function(x) (12*x)/(4+x^2)^(5/2)
exp <- integrate(j2, -Inf, Inf)$value

j3 <- function(x) (12*x^2)/(4+x^2)^(5/2)
exp2 <- integrate(j3, -Inf, Inf)$value

var <- exp2 - exp^2; var
#the variance here is 2

#Part f:
k1 <- function(x)x*dt(x,12)
exp <- integrate(k1, -Inf, Inf)$value; exp
#this tells that the expectation is 0

k2 <- function(x)x^2*dt(x,12)
exp2 <- integrate(k2, -Inf, Inf)$value; exp2

var <- exp2 - exp^2; var
#the variance calculated here is 1.2

k<- 12
varExpected <- k/(k-2); varExpected
#The two results match each other.