rm(list=ls())
# Import the Census data set. 
cen <- read.csv(file.choose(), header = T)

# Extract its columns.
age <- cen$AGE; head(age)
school <- cen$SCHOOL; head(school)
gain <- cen$GAIN; head(gain)
hrs <- cen$HOURS; head(hrs)
rich <- cen$RICH; head(rich)

#(Part a: [not specified this way in the question]) 
#probability of being rich~age

# Start with the log of the maximum likelihood function. 
lm <- function(alpha, beta) {
  -sum(log(exp(alpha+beta*age)/(1+exp(alpha+beta*age)))*rich+log(1/(1+exp(alpha+beta*age)))*(1-rich))
}
#logistic regression
library(stats4)
res <- mle(lm, start = list(alpha = 0, beta = 0)); res
#alpha = -2.74306323, beta = 0.03950995
#Plot
plot(age, rich, xlab = "Age", ylab = "Rich", main = "Rich as a Function of Age", col = "blue")
#logistic regression curve 
curve(exp(res@coef[1] + res@coef[2]*x)/(1 + exp(res@coef[1] + res@coef[2]*x)), col = "blue", add = TRUE)

#(Part b: [not specified this way in the question]) 
#Again, education~rich
#Plot
lm2 <- function(alpha, beta) {
  -sum(log( exp(alpha+beta*school)/(1+exp(alpha+beta*school)))*rich+log(1/(1+exp(alpha+beta*school)))*(1-rich))
}
plot(school, rich, xlab = "Education", ylab = "Rich", main = "Rich as a Function of Education", col = "blue")
res.ed <- mle(lm2, start = list(alpha = 0, beta = 0)); res.ed
#alpha = -5.0196729, beta = 0.3642899. 
#logistic regression
curve(exp(res.ed@coef[1] + res.ed@coef[2]*x)/(1 + exp(res.ed@coef[1] + res.ed@coef[2]*x)), col = "blue", add = TRUE)

#(Part c: [not specified this way in the question]) 
# Again, rich~capital gain.
# We start with the likelihood function. 
lm3<- function(alpha, beta) {
  -sum(log(exp(alpha+beta*gain)/(1+exp(alpha+beta*gain)) )*rich+log(1/(1+exp(alpha+beta*gain)))*(1-rich) )
}
#Plot
res.gain <- mle(lm3, start = list(alpha = 0, beta = 0)); res.gain
#alpha = -1.0506283900, beta = 0.0005468298
#Plot
plot(gain, rich, xlab = "Capital Gain", ylab = "Rich", main = "Rich as a Function of Capital Gain", col = "blue")
#logistic regresssion
curve(exp(res.gain@coef[1] + res.gain@coef[2]*x)/(1 + exp(res.gain@coef[1] + res.gain@coef[2]*x)), col = "blue", add = TRUE)