rm(list = ls())
#Part a:
install.packages("EnvStats")
library(EnvStats)
n <- 10000
y <- rpareto(n, 1, 4)
#show histogram
hist(y, probability = TRUE, col = "green", breaks="FD")
curve(dpareto(x,1, 4), col = "blue", lwd = 3, add = TRUE)

#Part b:
install.packages("fitdistrplus")
library(fitdistrplus)
data("danishuni")
ds <- danishuni
head(ds)
loss <- ds$Loss
#run a chi-square test
bins <- qpareto(0.1*(0:10), 1, 4); bins
bincode <- cut(loss, breaks = bins, labels = FALSE); bincode
sum(table(bincode))
length(loss)
#data points equalt to 1 need to be added back
bincode <- ifelse(loss == 1, 1, bincode); bincode
sum(table(bincode)); length(loss)
#create observed and expected vectors
obs <- as.vector(table(bincode)); obs
exp <- rep(sum(obs)/10, 10); exp
chisq <- sum((obs-exp)^2/exp); chisq
#calculate pvalue
#10 categories - equated actual and expected total - 2 imposed parameters
#10-3 = 7
pvalue <- pchisq(chisq, df = 7, lower.tail = FALSE); pvalue
#The pvalue is 0 here which means it is super low and is a very strong evidence 
#to reject the null hypothesis