### Armin (first: 2012-09-21; Last: 2012-09-25)
source("logLik.sempls.R")

### preperations
library("semPLS")
data(ECSImobi)
ecsi <- sempls(ECSImobi, mobi, wscheme="centroid")


### ic - information criteria
source("infCriteria.R")
source("saturate.R")

## All criteria
ic(ecsi, LV = "Satisfaction")
ic(ecsi, LV = "Loyalty")

## Final Prediction Error
ic(ecsi, LV = "Satisfaction", criteria = "FPE")


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
extractAIC(ecsi, LV = "Satisfaction") # yes, matches
extractAIC(m1)
stats::AIC(m1)
ic(ecsi, LV = "Satisfaction", criteria = "AIC2")

## Unbiased AIC
ic(ecsi, LV = "Satisfaction", criteria = "AICu")

## Corrected AIC
ic(ecsi, LV = "Satisfaction", criteria = "AICc")

## Bayesian IC
ic(ecsi, LV = "Satisfaction", criteria = "BIC")
ic(ecsi, LV = "Satisfaction", criteria = "BIC2")
stats::BIC(m1)

## Hannan Quinn
ic(ecsi, LV = "Satisfaction", criteria = "HQ")

## Corrected Hannan Quinn
ic(ecsi, LV = "Satisfaction", criteria = "HQc")

## CP
ic(ecsi, LV = "Satisfaction", criteria = "Cp")

## CP
ic(ecsi, LV = "Satisfaction", criteria = "GM")
