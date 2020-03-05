rm(list = ls())

data <- read.csv(file.choose(), header = T)
head(data)

#Part a:
temp <- data$BodyTemp
mean(temp)
var(temp)
#mean and variance for male and female respectively
muM<- mean(temp[data$Gender == "M"]); muM
varM<- var(temp[data$Gender == "M"]); varM

muF<- mean(temp[data$Gender == "F"]); muF
varF<- var(temp[data$Gender == "F"]); varF

#Part b:
#run permutation test to see statistical significance
#calculate observed difference in means
obs <- muF - muM; obs
#run the permutation test
N<- 10^4
diffs <- rep(0, N)

for(i in 1:N){
  gender <- sample(data$Gender)
  maleAv <- sum(temp*(gender=="M"))/sum(gender=="M")
  FeAv <- sum(temp*(gender=="F"))/sum(gender=="F")
  #Calculate the theoretical difference in permutation test
  diffs[i] <- FeAv - maleAv
}
hist(diffs, probability = TRUE, col = "green")
abline(v = obs, col = "red")
pvalue<- (sum(diffs>=obs)+1)/(N+1); pvalue
pvalueTwoSide <- pvalue*2; pvalueTwoSide

#only 2.279772...% of the permutated cases are as extreme
#as what is observed. As a result, the evidence is sufficient
#to reject the null hypothesis. The mean temperatures for men
#and women are the same.

#Part c:
#sample size is 65 men and women each
mean(diffs)
varDiffs <- (varM+varF)/sum(data$Gender=="M"); varDiffs
sdDiffs <- sqrt(varDiffs); sdDiffs
curve(dnorm(x, mean = 0, sd = sdDiffs), add = TRUE, col = "blue", lwd = 3)