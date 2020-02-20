rm(list = ls())

#The following line is provided in the question
p <- c((6/pi^2)*(1/(1:999)^2), .0006082)

#Part a:
#sum p and see if they add up to 17
sum(p)
#get expectation
exp <- sum((1:1000)*p); exp
expSqr <- sum((1:1000)^2*p); expSqr
(1000^2*p[1000])/expSqr

#Part b:
Y <- sample(x=1:1000, size=1000, prob=p, replace=TRUE)
hist(Y, breaks = 100, probability =TRUE, col = "blue")

#Part c:
flight <-read.csv(file.choose(), header =T); head(flight)
delay <- flight$Delay
hist(delay, breaks = "FD", prob = TRUE, cold = "red")
#The graph in part c resembles that in part b

#Part d:
X<-1:1000
#take two samples of X
#size of 10000
first<- sample(X, size=10000, prob=p, replace = TRUE)
second<- sample(X, size=10000, prob=p, replace = TRUE)
#compute the difference and store in K
k <- first - second; k
hist(k, breaks = 20, probability = TRUE, col = "blue")
#find the sample variance
var(k)
#var is a large value, therefore the game is fairly fair.