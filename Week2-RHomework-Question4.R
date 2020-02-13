battle <- read.csv(file.choose(), header = T); head(battle)

u1 <- "MEZU"
u2 <- "BRIG"

new <- subset(battle, (battle$Abbr == u1 | battle$Abbr == u2))
score <- new$Kills - new$Lost
ind <- which(new$Abbr == u1)
obs <- mean(score[ind]) - mean(score[-ind]); obs

#permutation test
n <- sum(new$Abbr == u1)
N <- 10^4
X <- rep(NA, N)

for (i in 1:N){
  ind <- sample(nrow(new), n)
  X[i] <- mean(score[ind]) - mean(score[-ind])
}

mean(X)
hist(X, breaks = 'FD')
abline(v = obs, col = 'red')

#p-values
pvalue <- (sum(obs>=X)+1)/(N+1); pvalue
twoTailed <- 2*pvalue; twoTailed

#By rule, this pair of resulting p-values (one-sided and two-sided)
#shows that the pattern is significant, thus rejecting the null
#hypothesis and accepting the alternative hypothesis (because two-tailed
#p-value I found, 0.03519648 is smaller than 0.05). This is to say that
#in terms of kills-lossesm one of these units is superior to the others

#ps: I was stuck on parts of this question, and was inspired by the office hour
#(credit to Mr. Micheal Liotti)