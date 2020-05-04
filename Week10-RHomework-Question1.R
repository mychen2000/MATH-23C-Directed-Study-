rm(list = ls())
data <- read.csv(file.choose(), header = T); head(data)

#Part a:
tSal <- data$Total; qb <- data$QB
cor(tSal,qb) #positively correlated
plot(qb, tSal, pch = ".", cex = 3)

#Part b:
QB.lm <- lm(tSal~qb, data =data);QB.lm;
a<- 19052.636
b<- 2.482
abline(a, b, col = "red")

#Part c:
rSquared <- (cor(tSal,qb))^2; rSquared
summary(QB.lm)
#because the score of correlation founded above is 0.5817139, 
#so the "Multiple R-sqaured" is equal to the square of the
#correlation as 0.3384.

#Part d:
v1 <- rep(1, nrow(data))
v2 <- qb - (sum(qb*v1)/sum(v1*v1))*v1
# Check for orthogonality
sum(v1*v2)
GS.b <- sum(tSal*v2)/sum(v2*v2); GS.b
# Project onto the axis to get the predicted y-values. 
PredY <- (sum(tSal*v1)/sum(v1*v1))*v1 + (sum(tSal*v2)/sum(v2*v2))*v2
# Recall that yhat = a + bx, so a = yhat - bx. 
GS.a <- PredY - GS.b*qb; head(GS.a)
# This gives a vector of intercepts, but every entry is equal. We can check
# this with the table() function.
table(GS.a)
# Let's extract as an object our intercept.
GS.a <- GS.a[1]; GS.a
#Both results for a and b here match the finding in Part b.
#a = 19052.6, b = 2.4817

#Part e:
#Find the regression line by projection.
A <- cbind(rep(1,nrow(data)),qb);head(A)
B <- t(A)%*%A; B
P <- A%*%solve(B)%*%t(A); P
y.hat <- P%*%tSal #predicted values, on the regression line 
plot(qb,tSal,pch = ".",cex = 3) #scatter plot of the data 
points(qb,y.hat,type = "b") #regression line
solve(t(A)%*%A)%*%t(A)%*%tSal
#The result here solves to be a = 19052.635718, b = 2.481745
#which agree with what were found in part b
