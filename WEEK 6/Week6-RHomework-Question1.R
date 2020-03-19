rm(list = ls())
A<- matrix(c(1,4,5,3,-2,0,-1,2,3,1,2,1,0,2,1,0), nrow=4); A

#create target
det(A) #this sets our goal

my.det <- function(A){
  if(length(A[,1])==2) return(A[1,1]*A[2,2]-A[1,2]*A[2,1]) 
  determ <- 0
  for (i in 1:length(a[,1])){
    determ <- determ - ((-1)^i) * A[i,1] * my.det(A[-i,1])
  }
  determ
}
#test with the matrix we created above
my.det(A)
#this produces a result of 40 that matches the expectation

#sum the permutation
library(combinat)
P <- permn(1:4); p
determin <- 0
for (i in 1:length(p)){
  sig <- P[[i]]
  determin <- determin + (-1)^(i+1)*A[1, sig[1]]*A[2, sig[2]]*A[3, sig[3]]
  *A[4, sig[4]]
}

#the determinant here is:
determin

##Column Reduction
detco <- 1

#swap
temp <- A[,2]
A[,2] <- A[,3]
A[,3] <-temp
A

#determinant
detco <- -1*detco; detco

#add -3 times column 1 to column 2
A[,2] <- -3*A[,1] + A[,2]; A
#add 2 times column 1 to column 3
A[,3] <- 2*A[,1] + A[,3]; A
#add column 2 to column 3
A[,3] <- A[,2] + A[,2]; A
#add 5 times column 4 to column 3
A[,3] <- 5*A[,4] + A[,3]; A
#add -1 times column 3 to column 4
A[,4] <- -1*A[,3] + A[,4]; A
#add 13 times column 3 to column 2
A[,2] <- 13*A[,3] + A[,2]; A
#add -5 times column 3 to column 1
A[,1] <- -5*A[,3] + A[,1]; A

temp2 <- A[,4]
A[,4] <- A[,2]
A[,2] <-temp
A

detco <- -1*detco; detco

#divide column 2 by -5
A[,2] <- (-1/5)*A[,2]; A
detco <- -5*detco; detco
#divide column 4 by -8
A[,4] <- (-1/8)*A[,4]; A
detco <- -8*detco; detco
#add 31 times column 2 to column 1
A[,1] <- 31*A[,2] + A[,1]; A
#add -7 times column 2 to column 3
A[,3] <- -7*A[,2] + A[,3]; A
#add 10 times column 2 to column 4
A[,4] <- 10*A[,2] + A[,4]; A
#add -3 times column 4 to column 1
A[,1] <- -3*A[,4] + A[,1]; A

#Therefore, the deteminant is
detco

#which is 40 as desired.
