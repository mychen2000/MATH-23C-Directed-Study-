sox <- read.csv(file.choose(), header = T); head(sox)
nats <- read.csv(file.choose(), header = T); head(nats)

#Part a:
X <- c(sox$R, sox$RA, nats$R, nats$RA)
barplot(table(X), xlab = "Number of Runs", ylab = "Number of games", col = "blue")

#Part b:
lambda <- mean(X); lambda #given that lambda is the eman
var(X)
#If the data is Possoin distribution, then var(X) should be equal to
#the lambda. However, since the values are very different from each otehr
#(4.738235 vs 11.14493), so Possoin is not appropriate here.

#Part c (I was pretty confused in the first place, so the solution to this
#part of the question was largely guided through in the office hour):
#Bin: 1, 2, 3,..., 13, >=14
observed <- rep(NA, 15)
for (i in 1:14){
  observed[i] <- table(X)[i]
}
observed; table(X)
observed[15] <- sum(table(X)[15:20]); observed
sum(observed)
N <- length(X); N

expected <- N*c(dpois(0:13, lambda), ppois(13, lambda, lower.tail = FALSE))
sum(dpois(0:13, lambda) + ppois(13, lambda, lower.tail = FALSE))
sum(expected)
#degree of freedom is 15-2=13
dfs <- 13
chisq <- sum((observed - expected)^2/expected); chisq
pvalue <- pchisq(chisq, dfs, lower.tail = FALSE); pvalue
#The p value found here is 2.168168*10^-227 which is super small and is
#sufficient to reject the null hypothesis which states that the data is
#from a Poisson distribution