rm(list = ls())

data <- read.csv(file.choose(), header = T)
head(data)

#precipitation column
precip <- data$precip
nrow(data); n <- length(precip); n

#plot
plot(1:n, precip, type = "l")

#cosine and sine fnctions:
Cos <- function(m) cos((1:n)*m*2*pi/n)
Sin <- function(m) sin((1:n)*m*2*pi/n)

#check the periodic pattern
plot(1:n, precip, type = "l")
points(1:n, 100*Cos(1), type = "l", col="blue")
points(1:n, 100*Sin(1), type = "l", col="green")
points(1:n, 100*Cos(10), type = "l", col="red")

#Fourier coefficients
coA <- function(m){
  sum(precip * Cos(m)/n)
}
coB <- function(m){
  sum(precip * Sin(m)/n)
}
ncoeff <- 10
#Compute the first 10 coefficients
FourierA <- sapply(1:ncoeff, coA)
FourierB <- sapply(1:ncoeff, coB)

#find the value of n
Fourier <- sqrt(FourierA^2 + FourierB^2)

#find index
which.max(Fourier)

#Plot
plot(1:n, precip, type = "l")
points(1:n, mean(precip)+ FourierA[7]*Cos(7)+FourierB[7]*Sin(7), type = "l", col = "yellow", lwd = 4)


#reconstruct
recon <- mean(precip)
for (m in 1:ncoeff){
  recon <- recon + FourierA[m]*Cos(m) + FourierB[m]*Sin(m)
}
#plot
plot(1:n, precip, type = "l")
points(1:n, recon, type = "l", col = "red", lwd = 4)
