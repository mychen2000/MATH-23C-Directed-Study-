#Part a: 
#m = 6, n = 20, k = 13 for a hypergeometric distribution
#X = 3, aka three trump cards
dhyper(3, 6, 20, 13) #matches the handwritten homework
#X = 2, aka three trump cards
dhyper(2, 6, 20, 13) #matches the handwritten homework
#X = 1, aka three trump cards
dhyper(1, 6, 20, 13) #matches the handwritten homework
#X = 0, aka three trump cards
dhyper(0, 6, 20, 13) #matches the handwritten homework
#All results match what I got in homework question 4

#Part b:
m <- 6
n <- 20
k <- 13

players <- c(rep("east", k), rep("west", k)); players
trumpCards <- c(rep(TRUE, m), rep(FALSE, n)); trumpCards
X <- rep(NA, 10^4)

for (i in 1:10^4){
  shuffle <- sample(trumpCards, m+n, replace = FALSE)
  X[i] <- sum(players=="east"&shuffle)
}

(table(X)[4])/(10^4)

#Part c:
cardsOfEast <- c(rep(TRUE, 13), rep(FALSE, 13))
isTrumpCard <- c(rep(TRUE, 2), rep(FALSE, 20), rep(TRUE, 4))
table(cardsOfEast, isTrumpCard)
#fisher sxact test:
fisher.test(cardsOfEast, isTrumpCard, alternative = "g")
#The result observes a p-value of 0.9199, which is to say that the probability that East has two or more trump cards is 91.99%
#This result also agrees with the dhyper() tyest (which is very close in terms of p-value)
1-dhyper(1, m, n, k)