rm(list = ls())

NC <- read.csv(file.choose(), header = T)
weight <- NC$Weight

#Part a:
N <- 10000
n <- 6
mu <- mean(weight); mu
sigma <- sd(weight); sigma
A <- rep(NA, N)

for (i in 1:N) {
  samp <- sample(weight, n)
  Xmean <- mean(samp)
  A[i] <- (Xmean - mu)/(sigma/sqrt(n))
}

#create and fit to the graph
hist(A, probability = TRUE, col="blue", breaks = "FD")
curve(dnorm(x,0,1), add = TRUE, col = "red", lwd = 2)

#Part b:
B <- rep(NA, N)
for (i in 1:N){
  samp <- sample(weight, n)
  stand <- (samp - mu)/sigma
  B[i] <- sum(stand^2)
}

hist(B, probability = TRUE, breaks = "FD", col = "blue")
curve(dchisq(x, n), add = TRUE, lwd = 2, col = "red")

#The graph and curve show a good fit.

# Part c:
C <- rep(NA, N)

for (i in 1:N) {
  samp <- sample(weight, n)
  s2 <- var(samp)
  C[i] <- (s2*(n-1))/sigma^2
}

hist(C, probability = TRUE, col ="blue", breaks = "FD")
curve(dchisq(x, n-1), add = TRUE, lwd = 2, col= "red")
#The graph and the curve fit each other well.

#Part d:
D <- rep(NA, N)

for (i in 1:N) {
  samp <- sample(weight, n)
  Xmean <- mean(samp)
  s <- sd(samp)
  D[i] <- (Xmean - mu)/(s/sqrt(n))
}

hist(D, probability = TRUE, col ="blue", breaks = "FD")
curve(dt(x, n-1), add = TRUE, lwd = 2, col= "red")
#this has a -1 degree freedom.
#The graph and the curve fit each other well.