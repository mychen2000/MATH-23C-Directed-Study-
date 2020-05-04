rm(list = ls())
cigCan <- read.csv(file.choose(), header = T); head(cigCan)
A <- cbind(cigCan$CIG, cigCan$BLAD, cigCan$LUNG, cigCan$KID, cigCan$LEUK)

#Part a:
m <- nrow(A); m
n <- ncol(A); n
s <- var(A); s
#find the rescaling factor
phi <- scale(A, center = TRUE, scale = c(rep(sqrt(m-1),n)))
phi

#Part b:
t(phi) %*% phi; s #These two values being equal to each other means that we can use Spectral Theorem
Eig <- eigen(s); Eig #Eigenvector
Eig$values #Eigenvalue
P <- Eig$vectors; P #matrix of eigenvectors
Pinv <- solve(P); Pinv #invert of P
Pinv %*% s %*% P #diagonal matrix of eigenvalues
eigen <- phi %*% P; eigen
# Create a new data frame, adding the coefficients
cigCan.eig <- data.frame(cigCan, v1 = eigen[,1], v2 = eigen[,2], v3 = eigen[,3], v4 = eigen[,4], v5 = eigen[,5]); head(cigCan.eig)


#Part c:
#By inspection, first eigenvector has the largest eigenvalue. 
Eig$values
#Since we observed the largest eigenvalue in the first eigenvector and
#there is a descending pattern of the eigenvalues. Therefore, we know
#that the first and second eigenvectors are the most and second most 
#correlated to the sales of cigarett.
Eig$vectors[,1]


#Part d:
A2 <- cbind(eigen[,1], eigen[,2], eigen[,3], eigen[,4], eigen[,5])
recon <- A2 %*% Pinv
#revserse the scaling and centering
original.s <- scale(recon, scale = c(rep(1/sqrt(m-1),n)))
original.c <- scale(original.s, center = -colMeans(A), scale = FALSE)
#evaluate
plot(A, original.c)
#reconstruct with two principal components. 
A3 <- cbind(eigen[,1], eigen[,2], 0, 0, 0) 
# Multiply by Pinv on the right.
recon <- A2 %*% Pinv
#revserse the scaling and centering
original.s <- scale(recon, scale = c(rep(1/sqrt(m-1), n)))
original.c <- scale(original.s, center = -colMeans(A), scale = FALSE)
#evaluate
plot(A,original.c)
#The result shows a fair match
# We did reasonably well, though the match is of course not perfect. 