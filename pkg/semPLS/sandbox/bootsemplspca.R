### Tests
library("semPLS")

data(ECSImobi)
Rprof()
ecsi <- sempls(model=ECSImobi, data=mobi, wscheme="pathWeighting")
Rprof(NULL)
summaryRprof()

Rprofmem("Rprofmem.out", threshold=1000)
ecsi <- sempls(model=ECSImobi, data=mobi, wscheme="pathWeighting")
Rprofmem(NULL)
noquote(readLines("Rprofmem.out", n=5))

set.seed(123)
ecsiBoot <- bootsempls(ecsi, nboot=200, start="ones", verbose=TRUE)
summary(ecsiBoot, type="perc", level=0.95)

tmp <- ecsiBoot$bootIndices
tmp <- cut(ecsiBoot$t[, "beta_6_7"], quantile(ecsiBoot$t[, "beta_6_7"], seq(0, 1, length=6)), include.lowest=TRUE)
levels(tmp) <- 1:5
tmp2 <- ecsiBoot$bootIndices
tmp3 <- data.frame(tmp2, tmp)

library("reshape")
tmp4 <- melt(tmp3, id = "tmp")
xtabs(~ value + tmp, data = tmp4)

indx <- apply(xtabs(~ value + tmp, data = tmp4), 2, which.max)

barplot(xtabs(~ value + tmp, data = tmp4)[c(242, 239), ], beside = TRUE)
barplot(xtabs(~ value + tmp, data = tmp4)[c(242, 239), ], beside = TRUE, legend = TRUE)

p1 <- prcomp(t(xtabs(~ value + tmp, data = tmp4)))
p1
summary(p1)
biplot(p1)
screeplot(p1)

parallelplot(ecsiBoot, pattern="beta", reflinesAt=c(0,1))
