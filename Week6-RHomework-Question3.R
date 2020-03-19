rm(list = ls())
# Import data
sat <- read.csv(file.choose(), header= T)
head(sat)

library(MASS)
#plot math v verbal scores
plot(sat$Math, sat$Verbal, pch = ".", cex = 3, asp = 1, col = "red")
mu.math <- mean(sat$Math); mu.math #613.4737
mu.verb <- mean(sat$Verbal); mu.verb #596.8421

#get vector
math <- sat$Math; verb <- sat$Verbal

#first row of covariance matrix
a <- cor(math, math)*sd(math)*sd(math); a
b <- cor(math, verb)*sd(math)*sd(verb); b
c <- cor(verb, math)*sd(verb)*sd(math); a
d <- cor(verb, verb)*sd(verb)*sd(verb); a

#create matrix
tab <- rbind(c(a,b), c(c, d))
tab

samp <- mvrnorm(n = 300, mu = c(mu.math, mu.verb), Sigma = tab)
samp

M <- samp[,1]; head(M)
V <- samp[,2]; head(V)
points(M, V, pch = ".", cex = 3, col = "green")

#The two data on the graph fit each other well.