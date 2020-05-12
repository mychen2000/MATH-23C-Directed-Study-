rm(list=ls())
#The dataset is a dataframe
#The dataset satisfies the requirements to have more than two numerical columns and more than two categorical columns
#It is named HRSurvey; load
hr <- read.csv(file.choose(), header = T); head(hr)
M<-nrow(hr)#So, we have 1470 rows of data, which exceeds teh row requirement by a lot

attach(hr)
dpmt <- Department
#barplot
barplot(table(dpmt))

#histogram
n<- 3
Expected <- rep(M/n, n)
Observed <- table(dpmt); Observed
chisq <- sum((Observed-Expected)^2/Expected); chisq
N <- 40000; diff <- numeric(N)
for (i in 1:N){
   v <- sample(1:n, M, replace = TRUE)
   Observed <- table(v)
   diff[i] <- sum((Observed-Expected)^2/Expected)
}
hist(diff, prob = TRUE)

#probability density graph
curve(dchisq(x, df = 2),add = TRUE, col = "red")

#contingency table
edf <- EducationField
table(dpmt, 
      edf) #contingency table

#The following lines of codes are the required analyses
#Let's start with the "Permutation Test"
#I want to find if there is statistically significance to make
#the claim that men make more money overall in IBM than women
#do. To do so, I will compare the hourly rate across the genders.
hrMale <- sum(hr$HourlyRate*(hr$Gender=="Male"))/sum(hr$Gender=="Male"); hrMale
hrFemale <- sum(hr$HourlyRate*(hr$Gender=="Female"))/sum(hr$Gender=="Female"); hrFemale
#Find if this difference is significant
hrDiff <- hrFemale-hrMale; hrDiff
#Repeat with a random sample, same size as the subset of men
samp <- sample(nrow(hr), sum(hr$Gender == "Male")); samp
#Find the mean for the random sample
hrSamp <- mean(hr$HourlyRate[samp]); hrSamp
hrOther <- mean(hr$HourlyRate[-samp]); hrOther
#Run the permutation test
diff <- hrSamp - hrOther; diff
#Once it works, do it 10000 times
N <- 10000
diff <- numeric(N) 
for (i in 1:N) {
   samp <- sample(nrow(hr), sum(hr$Gender == "Male"))
   hrSamp <- mean(hr$HourlyRate[samp]) #mean for the random sample
   hrOther <- mean(hr$HourlyRate[-samp]); #mean for the complement of the random sample 
   diff[i] <- hrSamp - hrOther
}
hist(diff)
abline(v= hrDiff, col = "red") #observed difference is way out on the tail
mean(diff >= hrDiff)
#The shown P-Value is 0.4928
#which means that there is a 49.28% chance that the distribution of the sample
#data is a result of pure chance. This result is at all insignificant to make the
#claim that women get paid less than men, at least under the setting of
#the expressed dataset of IBM

#Statistics based on a distribution function
#histogram
n<- 3
Expected <- rep(M/n, n)
Observed <- table(dpmt); Observed
chisq <- sum((Observed-Expected)^2/Expected); chisq
N <- 40000; diff <- numeric(N)
for (i in 1:N){
   v <- sample(1:n, M, replace = TRUE)
   Observed <- table(v)
   diff[i] <- sum((Observed-Expected)^2/Expected)
}
hist(diff, prob = TRUE)
#probability density graph
curve(dchisq(x, df = 2),add = TRUE, col = "red")
#Let's do a uniform distribution
dpmt
data <- hr$Department;data
Observed <- table(data); Observed
Expected <- rep(sum(Observed)/3,3); Expected
#490 for each category
chisq.value <- sum((Observed-Expected)^2/Expected); chisq.value
#the chi-squared value is 828.7878
hist(diff, prob = TRUE)
hist(diff, prob = TRUE, xlim = range(0,900))
curve(dchisq(x, df = 2),add = TRUE, col = "red")
abline(v = chisq.value, col = "blue" )
#The represented chi-squared value is so to the far right
sum(diff > chisq.value)/N
#In this simulation, the tested P-Value is 0
pchisq(chisq.value, df = 2, lower.tail = FALSE)
#And the theoretical p-value is also as small as 1.074053e-180
#This is way more than low enough to reject the null hypethesis
#Therefore, it is to show that the distribution of population
#into different job titles is not a random event.

#Analysis of a contingency table
#I am going to a chi-squre test of a contingency table
table(Gender, Attrition) #contingency table
#Use the function to do the Fisher Test
fisher.test(Gender, Attrition, alternative = "g")
#The test provides all of the following information based on the contingency table
#P-Value = 0.1452
#The alternative hypothesis can be that true odds ratio is greater than 1
#We are going to explore the attrition rate with the gender
#We can also do a muskins problem based on the contingency problem
#All of the following numbers can be calcualted through looking at the contingency table
m <- 237 #Attrition total
n <- 1233 #Not attrition
k <- 588 #Females
N <- 10000
AT <- numeric(N)
for (i in 1:N){
   scramble <- sample(Attrition, m+n, replace = FALSE) #permute the attrition
   AT[i] <- sum(Gender&scramble) 
}
hist(AT, breaks = 30)
chisq.test(Attrition, Gender) #we can also do a two-sided test
#We get the P-Value as 0.2906. This is to say that it is a 29.06% chance
#that such a distribution of Attrition in terms of Gender is totally by chance
#0.2906 is too big to reject the null hypothesis.

#Comparison of analysis by classical methods (chi-square, CLT) and simulation methods
#I am comparing a standard t-test to a permutation test
#This is a two-sided tests, so we consider evidence of significance
Automate <- function(x, y, alpha) { #x, y, and a significant level, alpha
   #This function is suggested by Mr. Michael Liotti
   Hypotheses <- list(
      Null.Hyp = "H0: The true population means are equal", 
      Alt.Hyp = "HA The true population means are not equal")
   xbar <- mean(x); ybar <- mean(y); diff <- ybar - xbar
   nx <- length(x); ny <- length(y)
   SVarx <- 0 
   
   #SVarx is the sample variance of x
   #Calculate the sample variance of y
   for (i in 1:nx) {
      iteration <- (x[i] - xbar)^2 / (nx - 1)
      SVarx <- SVarx + iteration
   }
   SVary <- 0 # Initialize at 0 
   
   #calculate the weighted average of the sample variances of x and y
   #compute t-statistics
   for (i in 1:ny) {
      iteration <- (y[i] - ybar)^2 / (ny - 1)
      SVary <- SVary + iteration
   }
   Pooled <- (nx - 1)/(nx + ny - 2) * SVarx + (ny - 1)/(nx + ny - 2) * SVary
   tstat <- (ybar - xbar)/sqrt(Pooled * (1/nx + 1/ny))
   df <- nx + ny - 2 #two-sided p-value. 
   # t-statistics is either positive or negative
   if (tstat > -tstat) { # i.e., the t-statistic is positive
      pvalue <- pt(-tstat, df) + pt(tstat, df, lower.tail = FALSE)
   }
   else # i.e., the t-statistic is negative 
      pvalue <- pt(tstat, df) + pt(-tstat, df, lower.tail = FALSE)
   # Test our null hypothesis. 
   if (pvalue < alpha) {
      TestPvalue <- "Reject the null hypothesis"
   }
   else {
      TestPvalue <- "Fail to reject the null hypothesis"
   }
   #Find the upper and lower limits
   LowerQuantile <- qt(alpha/2, df)
   UpperQuantile <- qt(1 - alpha/2, df)
   L <- (ybar - xbar) - sqrt(Pooled * (1/nx + 1/ny)) * UpperQuantile
   U <- (ybar - xbar) - sqrt(Pooled * (1/nx + 1/ny)) * LowerQuantile
   #Determine the interval based on the above limits
   ConfInterval <- c(L,U)
   #Determine the inclusiveness to decide whether to reject the null
   #hypothesis or not
   if (0 >= L & 0 < U) {
      TestCI = "Fail to reject the null hypothesis"
   }
   else {
      TestCI = "Reject the null hypothesis"
   }
   #find a region of observed values of the difference in which we would
   #not reject the null hypothesis
   A <- sqrt(Pooled * (1/nx + 1/ny)) * qt(alpha/2, df)
   B <- sqrt(Pooled * ((1/nx) + (1/ny))) * qt(1 - alpha/2, df)
   AcceptableRegion <- c(A,B)
   # Let's test the null hypothesis, using our observed difference 
   # in the sample means. 
   if (diff < A | diff > B) {
      TestRR <- "Reject the null hypothesis"
   }
   else {
      TestRR <- "Fail to reject the null hypothesis"
   }
   # Create a list to return. 
   Answer <- list(Hypotheses = Hypotheses, xbar = xbar, ybar = ybar, 
                  diff = diff, SVarx = SVarx, SVary = SVary, Pooled = Pooled, 
                  tstat = tstat, pvalue = pvalue, TestPvalue = TestPvalue, 
                  ConfInterval = ConfInterval, TestCI = TestCI, 
                  AcceptableRegion = AcceptableRegion, TestRR = TestRR)
   # Return this list.
   return(Answer)
}

confidence <- 0.95
alpha <- 1-confidence
x <- TotalWorkingYears
y <- MonthlyIncome
Automate(x, y, alpha)
#Afer running through the above comprehensive algorithm for a statistical test
#we get the following information (that are important for a statistical inference):
#T-Statistics = 52.86653
#P-Value = 0
#The null hypothesis is rejected with
#The confidence interval is (6250.882, 6732.421)
#This coincides with the simulation result
#Which has a acceptyable region of (-240.7695, 240.7695)
#The test result is also to reject the null hypothesis
#Therefore, we conclude that the Monthly Income of an
#employee at IBM is related to his/her total years of working
#This result is acuqred through running the above program that
#compares the performances of a Student T-Statistics and a permutation test