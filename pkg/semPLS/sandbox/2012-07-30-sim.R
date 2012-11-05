library("devtools")
load_all("semPLS")

### Simulation: Mode A

sim1 <- list()
class(sim1) <- "sempls"

mm <- cbind(from = paste("y", rep(1:4, each = 6), sep = ""),
            to = paste("x", 1:6, rep(1:4, each = 6), sep = ""))

sm <- cbind(from = c("y1", "y1", "y2", "y3"),
            to = c("y3", "y4", "y3", "y4"))

mvs <- mm[grep("x", mm)]
datdummy <- vector("list", length = length(mvs))
datdummy <- structure(datdummy, class = "data.frame", names = mvs)
datdummy
class(datdummy)

SIM1 <- plsm(datdummy, sm, mm)
sim1$model <- SIM1
sim1$outer_loadings <- SIM1$M
#sim1$outer_loadings <- edit(sim1$outer_loadings)
sim1$outer_loadings[sim1$outer_loadings == 1] <- rep(c(0.7, 0.8, 0.9), each = 2, times = 4)
sim1$cross_loadings <- sim1$outer_loadings
sim1$outer_weights <- sim1$outer_loadings
#sim1$outer_weights <- edit(sim1$outer_weights)
sim1$path_coefficients <- SIM1$D
sim1$path_coefficients[sim1$path_coefficients == 1] <- c(0.05, 0.35, 0.2, 0.6)
sim1$coefficients <- coef(sim1)

pathDiagram(sim1, file = "sim1", edge.labels = "both", graphics.fmt = "pdf")
system("xpdf sim1.pdf &")

opt <- options(error = recover) # setting the error option
library("mvtnorm")
sim1dat <- plsSimulator4(sim1, n = 1000)

sim1res <- sempls(SIM1, data = sim1dat)
pathCoeff(sim1)
pathCoeff(sim1res)

plsLoadings(sim1)
plsLoadings(sim1res)

set.seed(123)
sim1datL <- lapply(1:100,
                   function(x, object, n, ...){
                     plsSimulator4(object, n, ...)},
                   object = sim1, n = 1000)

sim1resL <- lapply(sim1datL,
                   function(data, model, ...){
                     sempls(model, data, ...)},
                   model = SIM1, verbose = FALSE)

sim1mres <- meanModel(sim1resL)

pathCoeff(sim1)
pathCoeff(sim1mres)

plsLoadings(sim1)
plsLoadings(sim1mres)

set.seed(123)
sim12datL <- lapply(1:100,
                   function(x, object, n, ...){
                     plsSimulator4(object, n, ...)},
                   object = sim1mres, n = 1000)

sim12resL <- lapply(sim12datL,
                   function(data, model, ...){
                     sempls(model, data, ...)},
                   model = SIM1, verbose = FALSE)

sim12mres <- meanModel(sim12resL)

pathCoeff(sim1)
pathCoeff(sim1mres)
pathCoeff(sim12mres)

set.seed(123)
sim13datL <- lapply(1:100,
                   function(x, object, n, ...){
                     plsSimulator4(object, n, ...)},
                   object = sim12mres, n = 1000)

sim13resL <- lapply(sim13datL,
                   function(data, model, ...){
                     sempls(model, data, ...)},
                   model = SIM1, verbose = FALSE)

sim13mres <- meanModel(sim13resL)

pathCoeff(sim1)
pathCoeff(sim1mres)
pathCoeff(sim12mres)
pathCoeff(sim13mres)


set.seed(123)
sim14datL <- lapply(1:100,
                   function(x, object, n, ...){
                     plsSimulator4(object, n, ...)},
                   object = sim13mres, n = 1000)

sim14resL <- lapply(sim14datL,
                   function(data, model, ...){
                     sempls(model, data, ...)},
                   model = SIM1, verbose = FALSE)

sim14mres <- meanModel(sim14resL)

pathCoeff(sim1)
pathCoeff(sim1mres)
pathCoeff(sim12mres)
pathCoeff(sim13mres)
pathCoeff(sim14mres)

plsLoadings(sim14mres)

set.seed(123)
sim15datL <- lapply(1:100,
                   function(x, object, n, ...){
                     plsSimulator4(object, n, ...)},
                   object = sim14mres, n = 1000)

sim15resL <- lapply(sim15datL,
                   function(data, model, ...){
                     sempls(model, data, ...)},
                   model = SIM1, verbose = FALSE)

sim15mres <- meanModel(sim15resL)

pathCoeff(sim1)
pathCoeff(sim1mres)
pathCoeff(sim12mres)
pathCoeff(sim13mres)
pathCoeff(sim14mres)
pathCoeff(sim15mres)

plsLoadings(sim14mres)
plsLoadings(sim15mres)

library(lattice)
library(reshape)
library(ggplot2)

oparams <- cbind(coef(sim1), type = "orig", name = rownames(coef(sim1)))
simparams <- cbind(do.call("rbind", lapply(sim1resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim")

simparams2 <- cbind(do.call("rbind", lapply(sim12resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim2")

simparams3 <- cbind(do.call("rbind", lapply(sim13resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim3")

simparams4 <- cbind(do.call("rbind", lapply(sim14resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim4")

simparams5 <- cbind(do.call("rbind", lapply(sim15resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim5")

params <- merge(oparams, simparams, all = TRUE)
params <- merge(params, simparams2, all = TRUE)
params <- merge(params, simparams3, all = TRUE)
params <- merge(params, simparams4, all = TRUE)
params <- merge(params, simparams5, all = TRUE)
head(params)

bparams <- subset(params, substring(name, 1, 4) == "beta")
p <- ggplot(data = bparams, aes(y = Estimate, x = type, col = type))
p + geom_boxplot(position = "identity") + facet_grid(. ~ Path)
dev.copy2pdf(file="standard_betas.pdf")

lparams <- subset(params, grepl("lam_1_[0-9]", name))
p <- ggplot(data = lparams, aes(y = Estimate, x = type, col = type))
p + geom_boxplot(position = "identity") + facet_grid(. ~ Path)
dev.copy2pdf(file="standard_lambdas.pdf")
save(params, file="standard_params.rda")


################################################################################
################################################################################
################################################################################

opt <- options(error = recover) # setting the error option
sim2dat <- plsSimulator3(sim1, n = 1000)
apply(sim2dat[["dataLV"]], 2, sd)
apply(sim2dat[["proxyLV"]], 2, sd)
#print(cor(sim2dat$dataMV), digits = 1)

sim2res <- sempls(SIM1, data = sim2dat$dataMV)
pathCoeff(sim1)
pathCoeff(sim2res)

plsLoadings(sim1)
plsLoadings(sim2res)

for(i in 1:4) print(cor(sim2dat[["dataLV"]][, i], sim2dat[["proxyLV"]][, i]))
for(i in 1:24) print(cor(sim2dat[["dataMV"]][, i], sim2dat[["dataMV2"]][, i]))


set.seed(123)
sim2datL <- lapply(1:100,
                   function(x, object, n, ...){
                     plsSimulator3(object, n, ...)},
                   object = sim1, n = 1000)

sim21resL <- lapply(sim2datL,
                   function(data, model, ...){
                     sempls(model, data[["dataMV"]], ...)},
                   model = SIM1, verbose = FALSE)

sim21mres <- meanModel(sim21resL)

pathCoeff(sim1)
pathCoeff(sim21mres)

plsLoadings(sim1)
plsLoadings(sim21mres)


sim22resL <- lapply(sim2datL,
                   function(data, model, ...){
                     sempls(model, data[["dataMV2"]], ...)},
                   model = SIM1, verbose = FALSE)

sim22mres <- meanModel(sim22resL)

pathCoeff(sim1)
pathCoeff(sim22mres)

plsLoadings(sim1)
plsLoadings(sim22mres)

library(lattice)
library(reshape)
library(ggplot2)

oparams <- cbind(coef(sim1), type = "orig", name = rownames(coef(sim1)))
simparams <- cbind(do.call("rbind", lapply(sim21resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim21")

simparams22 <- cbind(do.call("rbind", lapply(sim22resL,
                                           function(x){
                                             cbind(coef(x), name = rownames(coef(x)))}
                                           )),
                   type = "sim22")


params <- merge(oparams, simparams, all = TRUE)
params <- merge(params, simparams22, all = TRUE)
head(params)

bparams <- subset(params, substring(name, 1, 4) == "beta")
p <- ggplot(data = bparams, aes(y = Estimate, x = type, col = type))
p + geom_boxplot(position = "identity") + facet_grid(. ~ Path)

lparams <- subset(params, grepl("lam_1_[0-9]", name))
p <- ggplot(data = lparams, aes(y = Estimate, x = type, col = type))
p + geom_boxplot(position = "identity") + facet_grid(. ~ Path)

cor(sim2datL[[1]][["dataLV"]])
cor(sim2datL[[1]][["proxyLV"]])

d1 <- sapply(sim2datL, function(x) cor(x[["dataLV"]]))
r1 <- matrix(apply(d1, 1, mean), 4, 4); r1

d2 <- sapply(sim2datL, function(x) cor(x[["proxyLV"]]))
r2 <- matrix(apply(d2, 1, mean), 4, 4); r2

d3 <- sapply(sim2datL, function(x) cor(x[["dataLV"]], x[["proxyLV"]]))
r3 <- matrix(apply(d3, 1, mean), 4, 4); r3
r3[!diag(4)] <- 0

matrix(apply(d1, 1, mean), 4, 4) * 0.96^2
matrix(apply(d2, 1, mean), 4, 4)

r1
pathCoeff(sim1)
r2
crossprod(r3, r1) %*% r3

crossprod(solve(r3), r2) %*% solve(r3)

################################################################################
################################################################################
################################################################################

library("semPLS")
data(ECSImobi)
ecsi <- sempls(model=ECSImobi, data=mobi)

simmobi <- plsSimulator3(ecsi, n = 1000)

ecsisim <- sempls(model=ECSImobi, data=simmobi[["dataMV"]])

pathCoeff(ecsi)
pathCoeff(ecsisim)

r3 <- cor(simmobi[["dataLV"]], simmobi[["proxyLV"]])
r3[!diag(7)] <- 0
crossprod(solve(r3), pathCoeff(ecsisim)) %*% solve(r3)

l <- cor(simmobi[["dataMV"]], simmobi[["proxyLV"]])
class(l) <- "plsLoadings"
l

l <- cor(simmobi[["dataMV"]], simmobi[["dataLV"]])
class(l) <- "plsLoadings"
l

plsLoadings(ecsi)
plsLoadings(ecsisim)


set.seed(1514)
mobidataL <- lapply(1:200,
                   function(x, object, n, ...){
                     try(plsSimulator3(object, n, ...), silent=TRUE)},
                   object = ecsi, n = 1000)

ind <- sapply(mobidataL, inherits, what="try-error")
table(ind)
mobidataL <- mobidataL[!ind]
mobidataL <- mobidataL[1:100]

ecsisimL <- lapply(mobidataL,
                   function(data, model, ...){
                     sempls(model, data[["dataMV"]], ...)},
                   model = ECSImobi, verbose = FALSE)

ecsisimM <- meanModel(ecsisimL)

pathCoeff(ecsi)
pathCoeff(ecsisimM)

plsLoadings(ecsi)
plsLoadings(ecsisimM)
