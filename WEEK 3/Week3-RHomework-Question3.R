rm(list = ls())
#install and examine the data set "lottery"
install.packages("resampledata")
library(resampledata)
lot <- get("Lottery"); head(lot)
k <- nrow(lot); k
#set up observed values and expected values
obs <- as.vector(table(lot)); obs
n <- length(obs); n
exp <- k*c(rep(1/n, n)); exp
#find if the sum of expected values match the 500 in the sample
total <- sum(exp); total #matches
#compute chi square test
chisq <- sum((obs - exp)^2/exp); chisq
dfs <- n-1; dfs # only because of the k value, as the expected total
#is made equal to the actual total number
pvalue <- pchisq(chisq, dfs, lower.tail = FALSE); pvalue
#0.669616 (would be turned into a pretty large percentage) is not 
#sufficientto reject the null hypothesis that the data set is based 
#on a uniform distribution