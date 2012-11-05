### Armin
source("logLik.sempls.R")

### Examples
data(ECSImobi)
ecsi1 <- sempls(ECSImobi, mobi, wscheme="centroid")
ecsi2 <- sempls(ECSImobi, mobi, wscheme="factorial")
ecsi3 <- sempls(ECSImobi, mobi, wscheme="pathWeighting")

mapply(logLik, list(ecsi1=ecsi1, ecsi2=ecsi2, ecsi3=ecsi3))
mapply(logLik, list(ecsi1=ecsi1, ecsi2=ecsi2, ecsi3=ecsi3), LV = "Satisfaction")

AIC(ecsi1=ecsi1, ecsi2=ecsi2, ecsi3=ecsi3)
AIC(ecsi1=ecsi1, ecsi2=ecsi2, ecsi3=ecsi3, LV = "Satisfaction")

BIC(ecsi1=ecsi1, ecsi2=ecsi2, ecsi3=ecsi3)
BIC(ecsi1=ecsi1, ecsi2=ecsi2, ecsi3=ecsi3, LV = "Satisfaction")

deviance(ecsi)
deviance(ecsi, LV = "Satisfaction")

extractAIC(ecsi)
extractAIC(ecsi, LV= "Satisfaction")

