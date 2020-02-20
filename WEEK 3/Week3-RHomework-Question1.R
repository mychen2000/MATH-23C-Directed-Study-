rm(list = ls())

met <- read.csv(file.choose(), header = T)
head(met)
table(met$W.L)
wins <- (met$R>met$RA); length(wins); head(wins)
sum(wins)#this will actually result in 40 wins

#Part a:
#need to find won which games
index <- which(wins ==TRUE); index
#find the number of wins between two given games
X <- rep(NA, 40)
X[1] <- index[1]; X
for (i in 2:40){
  X[i] <- index[i] - index[i-1]
}
head(X); head(index)
#find the distribution of the data
table(X)
#set up observed and expected data
obs <- rep(NA, 4)
for(i in 1:2){
  obs[i]<-table(X)[i]
}
obs[3] <- table(X)[3] + table(X)[4]
obs[4] <- sum(table(X)[5:length(table(X))])
obs
sum(table(X))
sum(obs) #40 wins and match each other
#0, 1, 2, 3, >=4
exp <- rep(NA, 4)
p<- 1/4 #this is given
exp <- 40*c(dgeom(0:1, p), dgeom(2, p)+dgeom(3, p), 1-pgeom(3,p))
sum(exp) #40, matches the observed
barplot(rbind(obs, exp), beside = TRUE, col = c("blue","green"))
#chi-square test
chisq <- sum((obs - exp)^2/exp); chisq #1.788...
#a total of 4 categories, dfs is 4-2 =2
dfs <- 2
pvalue <- pchisq(chisq, dfs, lower.tail = FALSE); pvalue
#the pvalue is 0.4088347, which is 40.88% which is not small at all
#and thus cannot reject the null hythothesis that the data set here
#is from a geometric distribution

#Part b:
#number of games needed to get to the next win
streak <- rep(0, nrow(met)); length(streak)
streak[1] <- min(which(wins)); head(streak)
for (i in 2:nrow(met)){
  streak[i] <- min(which(tail(wins, 1-i)))
}
obs <- rep(0, 5)
obs[1:4] <- table(streak)[1:4]; obs
obs[5]<- sum(table(streak)[5:length(table(streak))])
obs
sum(obs)
#again, 1, 2, 3, >=4
n <- nrow(met); n
exp <- n*c(dgeom(0:3, p), pgeom(3, 1/4, lower.tail = FALSE))
exp
sum(exp) #161, matches that of the observed value
#run the chi-square test now:
dfs <- 5-2 # 5 categories, computed a parameter from data and made expected value equal to actual total
chisq <- sum((obs - exp)^2/exp); chisq
pvalue <- pchisq(chisq, dfs, lower.tail = FALSE); pvalue
#the p-value is 0.7029915 (70.30%), which is huge is is not sufficient at all to reject the null hypothesis
#which states that the data is from a chi-square distribution