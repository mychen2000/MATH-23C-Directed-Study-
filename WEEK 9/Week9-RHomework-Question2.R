rm(list = ls())
#observed temp
temp <- c(59, 58, 71, 80, 77, 67, 62, 58, 55)

#evaluate points:
eval <- 0:8

#create a pinv
pinv <- cbind(1, eval, eval^2, eval^3, eval^4, eval^5, eval^6, eval^7, eval^8); pinv

P <- solve(pinv); P

itp <- function(x, val) {
  c(1, x, x^2, x^3, x^4, x^5, x^6, x^7, x^8)%*%P%*%val
}

itp(17/3, temp)
#result reports 62.84198

#plot polynomial
#use 500 random values between 0 and 8
x <- seq(0, 8, length.out = 500)
fval <- sapply(x, itp, val = temp)
#plot
plot(x, fval, type = "l", xlab = "Every 3 Hours after 5", ylab = "Temperature")
#add specific points known:
points(eval, temp, pch = 24)
