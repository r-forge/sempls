# 23.05.2011
plsSimulator <- function(object, n, ordinal=TRUE, method=c("pearson", "kendall", "spearman"),
                         pairwise=FALSE, scale=c("scaled", "original"), total=FALSE, lc=FALSE)
{
  # Note: All MVs are treated as if they were reflective.
  scale <- match.arg(scale)
  method <- match.arg(method)
  ifelse(pairwise, use <- "pairwise.complete.obs", use <- "everything")
  model <- object$model
  blocks <- model$blocks
  data <- object$data
  exLVs <- exogen(model)
  endLVs <- endogen(model)
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

  #exLatent <- scale(simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE])
  exLatent <- simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE]


  # create endogen LVs from the model
  if(total){
    endLatent <- as.data.frame(exLatent %*% object$total_effects[exLVs,endLVs, drop=FALSE])
    rSq <- rSquared(object, total=total)
    for(i in endLVs){
      attr(endLatent[[i]], "R-squared") <- rSq[i,]
      if(lc){
        attr(endLatent[[i]], "load-correction") <- object$outer_weights[,i]
      }
      endLatent[[i]] <- addEps(endLatent[[i]], n=n)
    }
    endLatent <- as.matrix(endLatent)
  }
  else {
    Latent <- exLatent
    predList <- predecessors(model)
    rSq <- rSquared(object, total=total)
    for(i in endLVs){
      tmp <- Latent[, predList[[i]]] %*%
             object$path_coefficients[predList[[i]], i , drop=FALSE]
      if(lc){
        attr(tmp, "load-correction") <- object$outer_weights[,i]
      }
      attr(tmp, "R-squared") <- rSq[i,]
      tmp <- addEps(tmp, n=n)
      Latent <- cbind(Latent, tmp)
    }
    #endLatent <- as.data.frame(Latent[, endLVs])
    endLatent <- Latent[, endLVs]
  }


  # create endogen MVs from the model
  simendData <- endLatent %*%
                t(object$outer_loadings[endMVs, endLVs])
  # does the same:
  #simendData <- endLatent %*%
  #              t(object$outer_weights[endMVs, endLVs])
  simendData <- as.data.frame(simendData)

  loadings <- object$outer_loadings[endMVs,]
  loadings <- loadings[loadings!=0]
  names(loadings) <- endMVs

  for(i in endMVs){
    attr(simendData[[i]], "loadings") <- loadings[i]
    simendData[[i]] <- addEps(simendData[[i]], n=n)
  }

  simendData <- scale(simendData)
  simData <- cbind(simexData, simendData)
  if(scale=="original"){simData <- rescale(data, simData)}
  result <- simData
  return(result)
}

# function to computer signal to noise ratio from R squared
# (determination coefficient)
snr <- function(rSquared) sqrt(rSquared/(1-rSquared))

# function for adding error
addEps <- function(x, n){
  sigma <- sd(x)
  # for LVs
  if(!is.null(attr(x, "R-squared"))){
    if(!is.null(attr(x, "load-correction"))){
      lc <- sum(attr(x, "load-correction"))
    }
    #else lc <- 1.3 # seems to work (2011-05-23; AM: single case)
    #               # No, it does not! (2011-05-24; AM: simulation)
    else lc <- 1
    sigma <- sigma * (lc * snr(attr(x, "R-squared")))^-1
  }
  # for MVs
  else if(!is.null(attr(x, "loadings"))){
    ## theoretisch richtig
    sigma <- sigma * snr(attr(x, "loadings")^2)^-1
    ## eigentlich falsch, ABER?!?
    #sigma <- sigma/2 * snr(attr(x, "loadings")^2)^-1
  }
  else stop("Internal error.")
  x <- x + rnorm(n=n, mean=0, sd=sigma)
  return(x)
}
