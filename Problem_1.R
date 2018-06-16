## Homework 4, Problem 1

library(dplyr)

## Read the data
houseWork <-
  read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/houseWork.csv')
str(houseWork)

## a)
numWomen <- length(which(houseWork$sex == 'f'))
numMen <- length(which(houseWork$sex == 'm'))

## b)
manHours <- houseWork[houseWork$sex == 'm', 'hours']
meanManHours <- mean(manHours)

womanHours <- houseWork[houseWork$sex == 'f', 'hours']
meanWomanHours <- mean(womanHours)

## c)
houseWork <-
  within(houseWork, {
    female <- ifelse(houseWork$sex == 'f', TRUE, FALSE)
  })
houseWork <-
  within(houseWork, {
    male <- ifelse(houseWork$sex == 'm', TRUE, FALSE)
  })
## d)
fit <- lm(hours ~ female, data = houseWork)
## e)
## /score -2
# The coefficients mean that a) the overall weekly working hours (sex-independent) are
# equal to 32.81 and b) that if female working hours increase by 1%, the weekly working
# hours would change by -14.46 or decrease with 14.46. This also means that women work less
# on average than men
## f)
## /score -2
# h0 beta1 >=0
# the alternate is h1 beta1 < 0
# h0 postulates that women's working hours increase the overall average
# we've shown that the opposite is the case and so we reject the null hypothesis
## g)
## /score -2
popMean <- mean(houseWork$hours)
test <- sqrt(11016) & (womanHours - popMean) / 0.3186
pt(test, df = 11016)

## h)
## /score -2
## i)
## /score -2
# The test is for a t-distribution. Our data is expected to be normally distributed

## j)
## /score -2
fit1 <- lm(hours ~ female + male, data = houseWork)
summary(fit1)
