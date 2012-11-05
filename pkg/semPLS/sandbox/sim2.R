### Simulate data for semPLS
# mit selbst spezifiziertem Modell
library("semPLS")
sapply(list.files("../R", full = TRUE), source)

### function to calculate reflective MVs correlations
### from loadings vector
l2cor <- function(l) cov2cor(crossprod(t(l)) + diag((1-l^2)))
l2evar <- function(l) diag((1-l^2))
l2k <- function(l) diag(l^2)

this_n <- 100000
y1 <- rnorm(this_n)
l1 <- c(.8, .3, .5)
l2cor(l1)
l2 <- c(.9, .8, .7)
#l2n <- l2/norm(as.matrix(l2), "F")
l2n <- l2/sqrt(sum(l2cor(l2)))
l2n <- l2/sqrt(sum(crossprod(t(l2))))
l2n <- l2/sum(l2cor(l2))


dat2 <- rmvnorm(n=this_n, rep(0,3), diag(1, 3))
dat2 <- rmvnorm(n=this_n, rep(0,3), l2cor(l2n))
dat2 <- cbind(x1=addEps(l2n[1]*y1, n=this_n),
              x2=addEps(l2n[2]*y1, n=this_n),
              x3=addEps(l2n[3]*y1, n=this_n))

dat2 <- cbind(x1=addEps(l2[1]*y1, n=this_n),
              x2=addEps(l2[2]*y1, n=this_n),
              x3=addEps(l2[3]*y1, n=this_n))

cor(dat2)
cov(dat2)

lv1 <- dat2 %*% l2n
cor(data.frame(lv1=lv1, y1, dat2))

dat2_fa1 <- factanal(~ x1 + x2 + x3, data=data.frame(dat2), factors=1,
                     scores="Bartlett", rotation="none")
dat2_fa1
dat2_fa1$uniquenesses
sd(eps(.8, n=100000))
lv1a1 <- structure(dat2_fa1$scores, dimnames=list(dimnames(dat2_fa1$scores)[[1]], "FA"))
dat2_fa2 <- factanal(~ x1 + x2 + x3, data=data.frame(dat2), factors=1, scores="regression")
dat2_fa2
lv1a2 <- dat2_fa2$scores
cor(data.frame(lv1a=lv1a, y1, dat2))

dat2_pca <- prcomp(~ x1 + x2 + x3, data=data.frame(dat2), center=TRUE, scale=TRUE)
dat2_pca
lv1b <- predict(dat2_pca)[, "PC1"]
cor(data.frame(lv1=lv1, FA=lv1a1, PCA=lv1b, y1, dat2))


dat3 <- data.frame(lv1=lv1, y1=y1, dat2)

library("reshape")
dat4 <- melt(data=dat3, id.vars=c("y1", "lv1"), variable_name = "MV")
library("lme4")
op <- options(contrasts=c("contr.sum", "contr.poly"))

dat3_lmer <- lmer(value ~ -1 + (y1|MV) + y1:MV, data=dat4)
dat3_lmer
cor(dat3)
dat3_lmer <- lmer(value ~ -1 + (1 + lv1|MV) + lv1:MV, data=dat4)
dat3_lmer

### Next
pc1 <- matrix(0, ncol=2, nrow=2)
rownames(pc1) <- colnames(pc1) <- paste("y", 1:2, sep="")
pc1["y1", "y2"] <- 0.5
pc1

indsm <- which(pc1!=0, arr.ind=TRUE)
sm1 <- cbind(rownames(pc1)[indsm[,1]], colnames(pc1)[indsm[,2]])
colnames(sm1) <- c("source", "target")
sm1

oL1 <- matrix(0, nrow=3*nrow(pc1), ncol=nrow(pc1))
colnames(oL1) <- colnames(pc1)
rownames(oL1) <- paste("x", rep(1:2, each=3), 1:3, sep="")
j <- 1
for(i in colnames(oL1)){
  oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.7, .7, .7)
  #oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.9, .8, .7)
  #oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.8, .3, .5)
  j <- j+1
}
oL1

indmm <- which(oL1 != 0, arr.ind=TRUE)
mm1 <- cbind(rownames(oL1)[indmm[,1]], colnames(oL1)[indmm[,2]])
mm1

dat1 <- as.data.frame(matrix(NA, nrow=1000, ncol=nrow(oL1)))
names(dat1) <- rownames(oL1)
M1 <- plsm(dat1, strucmod=sm1, measuremod=mm1)
M1 <- invertLVs(model=M1, LVs=colnames(oL1)[1:2])
M1
exLVs <- exogenous(M1)

r1 <- matrix(c(1, .2, .4,  .2, 1, .3, .4, .3, 1), nrow=3, ncol=3, byrow=TRUE)
r1[as.logical(diag(1, 3))] <- 1
r1

r1 <- matrix(.01, nrow=3, ncol=3, byrow=TRUE)
r1[as.logical(diag(1, 3))] <- 1
r1

r1 <- l2cor(c(.7,.7,.7))
r1

set.seed(1)
library("mvtnorm")
dat1[, unlist(M1$blocks[[exLVs]])] <- rmvnorm(nrow(dat1), mean=rep(0, ncol(r1)), sigma=r1)
r2 <- l2cor(oL1[1:3, 1])
dat1[, unlist(M1$blocks[[exLVs]])] <- rmvnorm(nrow(dat1), mean=rep(0, ncol(r1)), sigma=r2)


M1m <- list()
M1m$model <- M1
M1m$path_coefficients <- pc1
M1m$outer_loadings <- oL1
M1m$outer_weights <- apply(oL1, 2, l2weights)
M1m$data <- scale(dat1)
class(M1m) <- "sempls"


m1k <- plsSimulator(M1m, n=1000, ordinal=FALSE, pairwise=FALSE, scale="scaled", analytical=TRUE)
m1k <- plsSimulatorOld(M1m, n=1000, ordinal=FALSE, pairwise=FALSE, scale="scaled")
apply(m1k$dataMV, 2, sd)
apply(m1k$dataLV, 2, sd)
m1k <- plsSimulator(M1m, n=1000, ordinal=FALSE, pairwise=FALSE, scale="scaled", analytical=FALSE)
apply(m1k$dataMV, 2, sd)
apply(m1k$dataLV, 2, sd)

M1mSim <- sempls(M1, m1k$dataMV, E="C", silent=TRUE)
M1bmSim <- sempls(M1b, m1k$dataMV, E="C", silent=TRUE)

M1m$path_coefficients
pathCoeff(M1mSim)

plsLoadings(M1mSim)
M1m$outer_loadings

set.seed(110607)
simM1_List<- list()
for(i in 1:100){
  print(i); #print(Sys.time())
  dat1[, unlist(M1$blocks[[exLVs]])] <- rmvnorm(nrow(dat1), mean=rep(0, ncol(r1)), sigma=r1)
  M1m$data <- scale(dat1)
  m1k <- plsSimulator(M1m, n=1000, ordinal=FALSE, pairwise=FALSE, scale="scaled", analytical=TRUE)
  print(apply(m1k$dataLV, 2, sd))
  simM1_List[[i]] <- sempls(M1, m1k$dataMV, E="C", silent=TRUE)
}

M1mSim100 <- meanModel(simM1_List)
M1m$path_coefficients
M1mSim100$path_coefficients

M1m$outer_loadings
plsLoadings(M1mSim100)
print(plsLoadings(M1mSim100), reldif=0.9)

simM2_List<- list()
for(i in 1:100){
  print(i); #print(Sys.time())
  m1k <- plsSimulator(M1mSim100, n=1000, ordinal=FALSE, pairwise=FALSE, scale="scaled", analytical=FALSE, verbose=TRUE)
  print(apply(m1k$dataLV, 2, sd))
  simM2_List[[i]] <- sempls(M1, m1k$dataMV, E="C", silent=TRUE)
}

M2mSim100 <- meanModel(simM2_List)
M1m$path_coefficients
M1mSim100$path_coefficients
M2mSim100$path_coefficients

M1m$outer_loadings
plsLoadings(M1mSim100)
plsLoadings(M2mSim100)
print(plsLoadings(M1mSim100), reldif=0.5)
