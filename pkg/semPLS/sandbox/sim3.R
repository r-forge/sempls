### Simulate data for semPLS
# mit selbst spezifiziertem Modell
library("semPLS")
sapply(list.files("../R", full = TRUE), source)

### function to calculate reflective MVs correlations
### from loadings vector
l2cor <- function(l) cov2cor(crossprod(t(l)) + diag((1-l^2)))
l2evar <- function(l) diag((1-l^2))
l2k <- function(l) diag(l^2)
l2weights <- function(l) l/sqrt(sum(l %*% l2cor(l)%*% l))

this_n <- 100000
y1 <- rnorm(this_n)

l2 <- c(.9, .9, .9)

dat2 <- cbind(x1=addEps(l2[1]*y1, n=this_n),
              x2=addEps(l2[2]*y1, n=this_n),
              x3=addEps(l2[3]*y1, n=this_n))

m1 <- lm(cbind(x1, x2, x3) ~ -1 + y1, data=data.frame(dat2))
dat2_hat <- predict(m1)


lv1 <- scale(dat2_hat) %*% l2 / sqrt(sum(crossprod(t(l2))))
#lv1 <- rowSums(dat2hat) / sqrt(sum(crossprod(coef(m1))))
cov(data.frame(lv1=lv1, y1, dat2))

dat2_fa1 <- factanal(~ x1 + x2 + x3, data=data.frame(dat2), factors=1,
                     scores="Bartlett", rotation="none")
dat2_fa1
dat2_fa1$uniquenesses
sd(eps(.9, n=100000))
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
pc1["y1", "y2"] <- 1
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
  #oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.7, .7, .7)
  #oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.9, .8, .7)
  #oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.8, .2, .6)
  oL1[paste("x", rep(j, each=3), 1:3, sep=""), i] <- c(.95, .95, .95)
  j <- j+1
}
oL1
l2 <- oL1[1:3,1]

indmm <- which(oL1 != 0, arr.ind=TRUE)
mm1 <- cbind(rownames(oL1)[indmm[,1]], colnames(oL1)[indmm[,2]])
mm1


M1 <- plsm(dat1, strucmod=sm1, measuremod=mm1)
M1 <- invertLVs(model=M1, LVs=colnames(oL1)[1:2])
M1
exLVs <- exogenous(M1)


y1 <- rnorm(this_n)
dat1 <- data.frame(x11=addEps(l2[1]*y1, n=this_n),
                   x12=addEps(l2[2]*y1, n=this_n),
                   x13=addEps(l2[3]*y1, n=this_n)
                   )
dat1$x21 <- dat1$x11
dat1$x22 <- dat1$x12
dat1$x23 <- dat1$x13

#m1 <- lm(cbind(x11, x12, x13) ~ -1 + y1, data=dat1)
#dat2 <- dat1
#dat2[,1:3] <- predict(m1)
#dat2[,4:6] <- predict(m1)

cov(dat1[, 1:3])
#cov(dat2[, 1:3])

m1sim <- sempls(M1, dat1, E="B", maxit=100)
m2sim <- sempls(M2, dat1, E="B", maxit=100)

pathCoeff(m1sim)
pathCoeff(m2sim)
plsLoadings(m1sim)
plsLoadings(m2sim)
oL1

f1 <- m1sim$factor_scores[,1]

f2 <- prcomp(~ x11 + x12 + x13, data=dat1, retx=TRUE)
f3 <- factanal(~ x11 + x12 + x13, factors=1, data=dat1, scores = "Bartlett")

#cov(cbind(y1, f1, f2$x[,1], f3$scores))
cor(cbind(y1, f1, PC1=f2$x[,"PC1"], f3$scores, dat1[, 1:3]))
