quiz1 <- c(100, 0, 0, 0)
quiz2 <- c(100, 0, 0, 0)
allPossible <- expand.grid(quiz1, quiz2); allPossible
allPossible <- data.frame(allPossible[, c(2:1)]); allPossible
colnames(allPossible) <- c("Quiz 1", "Quiz 2"); allPossible

attach(allPossible)

#The following line of code produces the average scores:
allPossible$X <- (`Quiz 1`+ `Quiz 2`)/2; allPossible

#The following lines of code produce the improvement scores:
dif <- (`Quiz 2`- `Quiz 1`)
i <- 1
impr <- rep(NA, nrow(allPossible))
while (i<=nrow(allPossible)){
  if (dif[i]>0){
    impr[i] <- 2
  }
  else if (dif[i]==0){
    impr[i] <- 1
  }
  else if (dif[i]<0){
    impr[i] <- 0
  }
  i <- i+1
}
allPossible$Y <- impr; allPossible

#The following line of code produces the value(s) of E(XY)-E(X)(Y):
mean(allPossible$X*allPossible$Y) == mean(allPossible$X)*mean(allPossible$Y)
#TRUE, so X and Y are uncorrelated

#The following line of code produces the value(s) of E(X^2 * Y^2)-E(^2X)*(Y^2):
mean((allPossible$X)^2*(allPossible$Y)^2) == mean((allPossible$X)^2)*mean((allPossible$Y)^2)
#FALSE, so X and Y are not independent

#Invent events A and B:
#Event A being X > 50:
allPossible$A <- ifelse(allPossible$X>50, 1, 0)
#Event B being Y== 1:
allPossible$B <- ifelse(allPossible$Y==1, 1, 0); allPossible
#P(A) = 1/16; P(B) = 10/16; P(A and B) = 1/16 != P(A)P(B) = 10/256. A and B satisfy the requirements.

detach(allPossible)