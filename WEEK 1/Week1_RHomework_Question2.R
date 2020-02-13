#Following line of code answers Part (a)
GSSLogical <- read.csv("GSSLogical.csv"); GSSLogical
head(GSSLogical)
AnswerToPartA <- subset(GSSLogical, (Male == FALSE)&(Republican == FALSE)&(GunOwner == TRUE)); NROW(AnswerToPartA) #prints the number of participants in the survey who are female Democrats who own guns

#Following line of code answers Part (b)
RedSox2013 <- read.csv("RedSox2013.csv"); RedSox2013
tenMoreRuns <- subset(RedSox2013, (R+RA)>=10); tenMoreRuns
min(tenMoreRuns$Duration) #Prints the shortest duration of a game in which two teams score a sume of 10 or more runs

#Following line of code answers Part (c)
attach(RedSox2013)
boxplot(R~DayNight, names=c("Day", "Night"), xlab="Time of the Day", ylab="Number of Runs", main="Number of Runs Scored by the Sox in Day Games vs in Night Games") #produces the desired boxgraph

#Following line of code answers Part (d)
##WonLost vs DayNight
pWon <- mean(WonLost == "W"); pWon
pDay <- mean(DayNight == "D"); pDay
total <- nrow(RedSox2013)
expectedDNWL <- total*outer(c(pDay, 1-pDay), c(1-pWon, pWon))
colnames(expectedDNWL) <- c('Lost', 'Won')
rownames(expectedDNWL) <- c('Day', 'Night'); expectedDNWL #calculated expected values
observedDNWL <- table(DayNight, WonLost)
colnames(observedDNWL) <- c('Lost', 'Won')
rownames(observedDNWL) <- c('Day', 'Night'); observedDNWL #actual values
chisq <- sum((observedDNWL-expectedDNWL)^2/expectedDNWL); chisq
pValue <- 1-pchisq(chisq, 1); pValue
chisq.test(DayNight, WonLost, correct=FALSE)
##WonLost vs Away
pWon <- mean(WonLost == "W"); pWon
pAway <- mean(Away == TRUE); pAway
expectedHAWL <- total*outer(c(1-pAway, pAway), c(1-pWon, pWon))
colnames(expectedHAWL) <- c('Lost', 'Won')
rownames(expectedHAWL) <- c('Home', 'Away'); expectedHAWL #calculated expected values
observedHAWL <- table(Away, WonLost)
colnames(observedHAWL) <- c('Lost', 'Won')
rownames(observedHAWL) <- c('Home', 'Away'); observedHAWL #actual values
chisq <- sum((observedHAWL-expectedHAWL)^2/observedHAWL); chisq
pValue <- 1-pchisq(chisq, 1); pValue
chisq.test(Away, WonLost, correct=FALSE)
#firstly, the chi-square value is big, which implies that the likelihood that the difference between the observed and expected values is purely by chance is pretty high
#the pretty big p-value, too, does not reject the null hyphothesis, which means there is little to do between Away and WonLost

detach(RedSox2013)