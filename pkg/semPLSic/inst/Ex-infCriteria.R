### Armin (first: 2012-09-21; Last: 2013-01-23)
### preperations

library("semPLS")
data(ECSImobi)
ecsi <- sempls(ECSImobi, mobi, wscheme="centroid")


## Adjusted R-sq; This value should match the one given by sempls package.
ic(ecsi, LV = "Satisfaction", criteria = "AdjRsq")
dat <- data.frame(ecsi$factor_scores)
m1 <- lm(Satisfaction ~ - 1 + Image + Expectation + Quality + Value, dat)
summary(m1)$adj.r.squared    # should have given the same result
m2 <- update(m1, . ~ . + 1)  # maybe with intercept
summary(m2)$adj.r.squared    # No
rSquared2(ecsi)["Satisfaction", "R-squared-corrected"]

## AIC
ic(ecsi, LV = "Satisfaction", criteria = "AIC")
semPLS:::extractAIC.sempls(ecsi, LV = "Satisfaction") # yes, matches
stats::extractAIC(m1)
stats::AIC(m1)
ic(ecsi, LV = "Satisfaction", criteria = "AIC2")

## Bayesian IC
semPLS:::ic.sempls(ecsi, LV = "Satisfaction", criteria = "BIC")
semPLS:::ic.sempls(ecsi, LV = "Satisfaction", criteria = "BIC2")
stats::BIC(m1)
