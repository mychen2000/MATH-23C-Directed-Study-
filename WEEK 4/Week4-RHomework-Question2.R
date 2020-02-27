rm(list = ls())

#Part a:
library(resampledata)
head(Service)
#extract time
times <- Service$Times; head(times)
hist(times, probability = TRUE, col = "blue", breaks = "FD")

#Part b:
#save gamma parameter
r <- 2.65; lambda <- 3.81
curve(dgamma(x, shape = r, rate = lambda), add = TRUE)

#Part c:
#mean and variance
mean <- mean(times); mean
vari <- var(times); vari
#use integration to compute expectation
f <- function(x){
  x*dgamma(x, shape = r, rate = lambda)
}
exp <- integrate(f, 0, Inf); exp; mean
F2 <- function(x){
  x^2*dgamma(x, shape = r, rate = lambda)
}
exp2 <- integrate(F2, 0, Inf); exp2
exp2$value-(exp$value)^2; vari
#expectation and variance for this gamma distribution match the data well

#Part d:
#degree of freedom:
#10 bins -1 -2 = 7 degrees of freedom
bins <- qgamma(0.1*(0:10), r, lambda); bins
#find the bin where the data goes
bincode <- cut(times, bins, labels = FALSE); bincode
obs <- as.vector(table(bincode)); obs
exp <- rep(sum(obs)/10, 10); exp
#chi square
chisq <- sum((obs-exp)^2/exp); chisq
pvalue <- pchisq(chisq, df = 7, lower.tail = FALSE); pvalue
#the p value of 0.9611... = 96.11...% is not sufficient to 
#reject the null hypothesis that the data is from a gamma 
#distribution