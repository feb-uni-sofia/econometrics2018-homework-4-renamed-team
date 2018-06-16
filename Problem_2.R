## Homework 4, Problem 2
crime <-
  read.delim(
    'https://s3.eu-central-1.amazonaws.com/econometrics2018/data/crime.csv',
    stringsAsFactors = FALSE
  )
str(crime)

## a)
fit <- lm(C ~ HS, data = crime)
summary(fit)
## /score -0.5 not specific enough
# As the number of high school graduates increase, so do the crime rates
## b)
pairs( ~ C + U + I + HS, data = crime)
## c)
fit1 <- lm(C ~ HS + U, data = crime)
summary(fit1)
## /score -0.5 not speficif enough!
# There is multicolinearity between feature variables, which affects the estimates. This means
# that feature variables affect each other and that the estimates for the explained variable
                                        # may be off.

## d)
## /score -1 not specific enough!
# Multicolinearity must be taken into account - it is not evident that crime rates are *caused* by high school
# graduation rates - it is likely affected by other factors.
# And considering other benefits of education, the politician's proposition is dubious
# at best
## e)
fit2 <- lm(C ~ HS + U + I, data = crime)
summary(fit2)

## /score -5
