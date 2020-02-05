head(airquality)
attach(airquality)
#Part (a)
boxplot(Ozone ~ Month, na.rm = TRUE)
boxplot(Solar.R ~ Month, na.rm = TRUE)
boxplot(Wind ~ Month, na.rm = TRUE)
boxplot(Temp ~ Month, na.rm = TRUE)
#Part (b)
plot(Temp, Ozone, na.rm = TRUE) # the points look to be arranged in a certain pattern, so I'd say there is some correlation, but maybe not linear
modelTO <- lm(Solar.R ~ Temp, na.rm = TRUE)
abline(modelTO$coefficients[1], modelTO$coefficients[2])
plot(Temp, Solar.R, na.rm = TRUE) # the points look randomly scattered around, so I'd say there is no clear correlation
modelTS <- lm(Solar.R ~ Temp, na.rm = TRUE)
abline(modelTS$coefficients[1], modelTS$coefficients[2])
#Part (c)
averageOzone <- mean(Ozone, na.rm = TRUE); averageOzone
averageSolar.R <- mean(Solar.R, na.rm = TRUE); averageSolar.R
averageWind <- mean(Wind, na.rm = TRUE); averageWind
averageTemp <- mean(Temp, na.rm = TRUE); averageTemp
smoggy <- (Ozone>averageOzone); smoggy
sunny <- (Solar.R>averageSolar.R); sunny
windy <- (Wind>averageWind); smoggy; windy
hot <- (Temp>averageTemp); smoggy; hot
allJudgement <- (smoggy&sunny&windy&hot); allJudgement
allTrue <- subset(airquality, allJudgement, select=c("Month", "Day")); allTrue # prints months and days when all four are true
#Part (d) # since p values measure how likely there is no association between variables
chisq.test(Wind, Month, correct = FALSE) # a fairly high p-value in this pair shows that Wind and Month do not have significant association (Wind and Month appear to be independent)
chisq.test(Temp, Month, correct = FALSE) # a low p-value in this case well below 0.01 shows that Temp and Month are highly likely to have some association (Temp and Month appear to be not independent)