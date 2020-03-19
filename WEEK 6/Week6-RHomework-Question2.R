rm(list = ls())

#set the number of simulations
N <- 30000
#create vectors
milk <- numeric(N)
coffee <- numeric(N)
surlatte <- numeric(N)

for (i in 1:N) {
  milk[i] <- runif(1,2,6)
  coffee[i] <- runif(1,1,4)
  total <- milk[i] + coffee[i]
  ratio <- milk[i]/coffee[i]
  surlatte[i] <- (total >= 4) & (total <= 8) & (ratio >= 1) & (ratio <= 3)
}

#number of surviving lattes
sum(surlatte)

#proporation of lattes that survive
mean(surlatte)

#get a vector that is zeroed out
surmilk <- milk*surlatte; head(surmilk)
length(which(surmilk != 0)) /N
survcof <- coffee * surlatte; head(survcof)
length(which(survcof != 0))/N
Data <- data.frame(surmilk, survcof)
newdata <- subset(Data, (surmilk != 0) & (survcof != 0)) 
nrow(newdata)/N

#avaerage amount that survived
mean(newdata$surmilk)
mean(newdata$survcof)
total <- newdata$surmilk + newdata$survcof; head(total)
mean(total)
ratio <- newdata$surmilk/newdata$survcof; head(ratio)
hist(total, probability = TRUE, col = "blue")
hist(ratio, probability = TRUE, col = "green")