NC <- read.csv(file.choose(), header = 1); head = (NC)
#Part a
maleInd <- which(NC$Gender == "Male")
maleMean <- mean(NC$Weight[maleInd]); maleMean
femaleInd <- which(NC$Gender == "Female")
femaleMean <- mean(NC$Weight[femaleInd]); femaleMean
dif <- (maleMean-femaleMean); dif
#permutation test
N <- 10^4
perm <- rep(NA, N)
for (i in 1:N){
  gender <- sample(NC$Gender)
  meanM <- sum(NC$Weight*(gender =="Male"))/sum(gender == "Male")
  meanF <- sum(NC$Weight*(gender =="Female"))/sum(gender == "Female")
  perm[i] = meanM - meanF
}

mean(perm)
hist(perm, breaks = "FD")
abline(v = dif, col = "red")

pvalue <- (sum(perm>=dif)+1)/(N+1); pvalue
twoTailed <- 2*pvalue; twoTailed

#By running the permutation test code above twice and the two-tailed p-values for two trials, 
#I got 0.00119988 for the first time and 0.00079992 for the second time. Both are small numbers
#that are smaller than the conventional cut-off line of 0.05. So this permutation test rejects
#the null hypothesis, thus accepting the alternative hypothesis. In another word, male babies
#are born to be heavier than female babies according to statistics.

#Part b
tobInd  <- which(NC$Tobacco == "Yes"); tobInd
tobMean <- mean(NC$Weight[tobInd]); tobMean
notTobMean <- mean(NC$Weight[-tobInd]); notTobMean
difTob <- notTobMean - tobMean; difTob
#permutation test
Ntob <- 10^4
permTob <- rep(NA, N)
for (j in 1:N){
  tob <- sample(NC$Tobacco)
  meanY <- sum(NC$Weight*(tob =="Yes"))/sum(tob == "Yes")
  meanN <- sum(NC$Weight*(tob =="No"))/sum(tob == "No")
  permTob[j] = meanN - meanY
}

mean(permTob)
hist(permTob, breaks = "FD")
abline(v = dif, col = "red")
permTob
difTob

pvalueTob <- (sum(permTob>=difTob)+1)/(N+1); pvalueTob
twoTailedTob <- 2*pvalueTob; twoTailedTob

#By running the permutation test code above to get the two-tailed p-value, I got 0.00019998 for 
#the first trial, which is smaller than the conventional cut-off line of 0.05. So this permutation 
#test rejects the null hypothesis, thus accepting the alternative hypothesis. In another word, 
#both one-tail and two-tailed p-values assure by statistics that babies have more weight
#if their mother does not smoke during pregnancy.
