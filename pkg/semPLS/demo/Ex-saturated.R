### Example: saturated model
library("semPLS")
data(ECSImobi)

## is already saturated ...
saturate(ECSImobi, "Satisfaction")

##  ... this was not
LV <- "Value"
predecessors(saturate(ECSImobi, LV))[[LV]]
predecessors(ECSImobi)[[LV]]

LV <- "Loyalty"
predecessors(saturate(ECSImobi, LV))[[LV]]
predecessors(ECSImobi)[[LV]]
