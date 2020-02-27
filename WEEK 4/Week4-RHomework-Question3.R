rm(list = ls())

#Part a:
N <- 10^4 # number of trials
delta <- rep(0, N)
for (i in 1:N){ # interested in the length of the longest sub interval
  p <- sort(c(0, 1, runif(3)))
  delta[i]<- max(p[2]-p[1],p[3]-p[2],p[4]-p[3],p[5]-p[4])
}
hist(delta, probability = TRUE, col = "blue")
#expectation and variance
exp <- mean(delta); exp
var <- var(delta); var

#Part b:
f <- function(x){
  x^2
}
#create left Riemann sum
left <- function(p){
  #some partition p
  result <- 0
  #loop over the interval
  for (i in 1:4){
    result<-result + (f(p[i])*(p[i+1]-p[i]))
  }
  return(result)
}
#left Riemann sum
right <- function(p){
  #some partition p
  result <- 0
  #loop over the interval
  for (i in 1:4){
    result<-result + (f(p[i+1])*(p[i+1]-p[i]))
  }
  return(result)
}
#midpoint
mid <- function(p){
  #some partition p
  result <- 0
  #loop over the interval
  for (i in 1:4){
    result<-result + (f((p[i]+p[i+1])/2)*(p[i+1]-p[i]))
  }
  return(result)
}
#10000 trials; mid sum and histogram
N <- 10^4; Y <- rep(0, N)
for (i in 1:N){
  p <- sort(c(0,1,runif(3)))
  #create midpoint sum
  Y[i] <- mid(p)
}
hist(Y, probability = TRUE, breaks = "FD", col = "blue")

#Part c:
#Simpson's rule
simpsum <- function(p){
  trap <- (left(p)+right(p))/2
  return(1/3)*trap+(2/3)*mid(p) 
}
#check some partition
p <- sort(c(0,1, runif(3)))
simpsum(p)
#exactly 0.3333..., which is expected for the given integral
#in the question