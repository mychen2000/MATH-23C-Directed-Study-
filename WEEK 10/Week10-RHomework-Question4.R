rm(list=ls())
fbs <- read.csv(file.choose(), header = T); head(fbs)
ats <- fbs$Assets; sales <- fbs$Sales; mv <- fbs$Market_Value; pft <- fbs$Profits; cf <- fbs$Cash_Flow; emp <- fbs$Employees
FBS <- cbind(ats, sales, mv, pft, cf, emp); FBS
A <- var(FBS); A #covariance matrix of FBS

#Part a:
eigen(A)$vectors #eigenvector
eigen(A)$values #eigenvalue

#Part b:
f <- function(x) {sum(x * (A %*% x))}
N <- 10^5 #number of trials
rvec <- numeric(N)
X <- list(N)
#Trials in th R^6 space
for (i in 1:N) {
  x <- runif(6, min = -1, max = 1)
  x1 <- x/sqrt(sum(x*x))
  rvec[i] <- f(x1)
  X[[i]] <- x1
}
#Find largest eigenvalues by comparison
lambda <- max(rvec); lambda #the result is close to "eigen(A)$values[1]"
#eigenvector. 
v1 <- X[[which.max(rvec)]]; v1 #fairly close to the values of "eigen(A)$vectors[,1]"
#By comparison with the accurate results I know, the optimization algorithm above works quite well

#Part c:
P <- v1%*%solve(t(v1) %*% v1)%*%t(v1); P
I <- diag(6); I
#I - P
dif <- I - P; dif


#Part d:
#Find second largest eigenvalue via orthogonal subspace
N <- 10^5 #trials
rvec <- numeric(N); X <- list(N)
#trials
for (i in 1:N) {
  x <- rnorm(6)
  Ax <- dif%*%x
  u <- Ax/sqrt(sum(Ax*Ax))
  rvec[i] <- f(u)
  X[[i]] <- u
}
lambda2 <- max(rvec); lambda2 #second largesr
#Very close to the value "eigen(A)$values[2]"
v2 <- X[[which.max(rvec)]]; v2
#The corresponding eigenvector above is close to the value of "eigen(A)$vectors[,2]"
#We get reasonably close matches