# 10.06.2011
# Works: See '../../../ideas/sim3.R'
plsSimulator <- function(object, n, ordinal=TRUE, method=c("pearson", "kendall", "spearman"),
                         pairwise=FALSE, scale=c("scaled", "original"), verbose=FALSE, analytical=FALSE)
{
  # Note: All MVs are treated as if they were reflective.
  scale <- match.arg(scale)
  method <- match.arg(method)
  ifelse(pairwise, use <- "pairwise.complete.obs", use <- "everything")
  model <- object$model
  blocks <- model$blocks
  data <- object$data
  exLVs <- exogenous(model)
  endLVs <- endogenous(model)
  exMVs <- unlist(model$blocks[exLVs])
  endMVs <- unlist(model$blocks[endLVs])
  factor_scores <- object$factor_scores

  if(ordinal) {
    if(require(orddata)==FALSE){
      stop("If argument ordinal is set to TRUE the orddata package is required.")
    }
    exData <- as.data.frame(rescale(data)[, exMVs, drop=FALSE])
    simexData <- matrix(NA, nrow=n, ncol=length(exMVs))
    colnames(simexData) <- exMVs
    for(i in exLVs){
      r <- cor(exData[, blocks[[i]]], method=method, use=use)
      probs <- lapply(exData[, blocks[[i]]], function(x) table(x)/length(x))
      simexData[, blocks[[i]]] <- rmvord(n=n, probs=probs, Cor=r, showCor_norm = FALSE)
    }
    simexData <- scale(simexData)
  }

  if(!ordinal) {
    if(require(mvtnorm)==FALSE){
      stop("If argument ordinal is set to FALSE the mvtnorm package is required.")
    }
    exData <- as.data.frame(rescale(data)[, exMVs, drop=FALSE])
    simexData <- matrix(NA, nrow=n, ncol=length(exMVs))
    colnames(simexData) <- exMVs
    for(i in exLVs){
      r <- cor(exData[, blocks[[i]]], method=method, use=use)
      simexData[, blocks[[i]]] <- rmvnorm(n=n, mean=rep(0, nrow(r)), sigma=r)
    }
    simexData <- scale(simexData)
  }

  ##exLatent <- scale(simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE])
  #exLatent <- simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE]
  exLatent <- simexData %*% object$outer_loadings[exMVs, exLVs, drop=FALSE]
  for(LV in exLVs){
    if(dim(simexData[, blocks[[LV]]])[2] == 1) next
    l <- object$outer_loadings[blocks[[LV]], LV, drop=FALSE]
    L <- l %*% t(l)
    S <- cov(simexData[, blocks[[LV]]])
    exLatent[, LV] <- exLatent[, LV] / sqrt(sum(L * S))
  }


  # create endogenous data
  Latent <- exLatent
  predList <- predecessors(model)
  succList <- successors(model)
  simendData <- NULL
  #threeStep <- object$outer_loadings %*% object$path_coefficients %*% t(object$outer_loadings)
  for(LV in endLVs){
    for(MV in blocks[[LV]]){
      tmp <- Latent[, predList[[LV]]] %*% object$path_coefficients[predList[[LV]], LV, drop=FALSE]
      tmp <- tmp * object$outer_loadings[MV, LV]
      tmp <- addEps(tmp, n=n)
      colnames(tmp) <- MV
      simendData <- cbind(simendData, tmp)
    }
    if(dim(simendData[, blocks[[LV]], drop=FALSE])[2] == 1){
      tmp <- simendData[, blocks[[LV]]] %*% object$outer_loadings[blocks[[LV]], LV, drop=FALSE]
      Latent <- cbind(Latent, tmp)
      next
    }
    l <- object$outer_loadings[blocks[[LV]], LV, drop=FALSE]
    beta <- object$path_coefficients[predList[[LV]], LV, drop=FALSE]
    L <- l %*% t(l)
    S <- cor(simendData[, blocks[[LV]]])
    extra <- NULL
    res <- NULL
    for(i in rownames(beta)){
        #print(beta)
        l2 <- l * beta[i,]
        extra[i] <- sum(l2)
        L2 <- l2 %*% t(l2)
        if(is.null(res)) res <- L2
        else res <- res + L2
    }
    extraP <- prod(extra) * sum(object$path_coefficients[rownames(beta), rownames(beta)])
    if(length(extra)>1){
        prvCor <- cor(Latent[, rownames(beta)])
        prvCor[!upper.tri(prvCor, diag=FALSE)] <- 0
        cat("prvCor:\n")
        print(prvCor)
        cat("pc:\n")
        print(object$path_coefficients[rownames(beta), rownames(beta)])
        extraC <- prod(extra) * sum(prvCor)
        cat("extraC:\n")
        print(extraC)
        cat("extraP:\n")
        print(extraP)
        res <- res + extraC
    }
    res[as.logical(diag(1, dim(res)[1]))] <- 1
    cat("S:\n")
    print(S)
    cat("Lbeta:\n")
    print(res)
    if(analytical) S <- res
    tmp <- simendData[, blocks[[LV]]] %*% object$outer_loadings[blocks[[LV]], LV, drop=FALSE]
    tmp <- tmp / sqrt(sum(L * S))
    Latent <- cbind(Latent, tmp)
  }

  simData <- cbind(simexData, simendData)
  #colnames(simData) <- colnames(data)
  result <- list(dataMV=simData, dataLV=Latent)
  return(result)
}

## function to computer signal to noise ratio from R squared
## (determination coefficient)
snr <- function(rSquared) sqrt(rSquared/(1-rSquared))


## function for adding error
addEps <- function(x, n){
  if(var(x) > 1) stop("MVs variance exceeds 1.\nIllegal parameter choices.")
  sigma <- sqrt(1- var(x))
  x <- x + rnorm(n=n, mean=0, sd=sigma)
  return(x)
}

## rescale() is missing
# rescale <- function(){}

