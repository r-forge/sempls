# 2012-07-31
# in constructiion
plsSimulator4 <- function(object, n, ...)
{
  # only for reflective measures (standard sem procedure)
  # get information from 'object'
  L <- object$outer_loadings
  B <- object$path_coefficients

  phi <- B + t(B)
  diag(phi) <- 1
  sigma <- L %*% tcrossprod(phi, L)
  diag(sigma) <- 1
  dat <- as.data.frame(rmvnorm(n, mean = rep(0, nrow(L)), sigma = sigma, ...))
  names(dat) <- rownames(L)
  return(dat)
}

plsSimulator3 <- function(object, n, ...){
  # get information from 'object'
  model <- object$model
  blocks <- model$blocks
  exLVs <- exogenous(model)
  endLVs <- endogenous(model)
  exMVs <- unlist(blocks[exLVs])
  endMVs <- unlist(blocks[endLVs])
  L <- object$outer_loadings
  B <- object$path_coefficients
  W <- object$outer_weights
  # list of correlation Matrizes for exogenous, formative MVs
  S <- object$formative


  ##exLatent <- scale(simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE])
  #exLatent <- simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE]
  #exLatent <- simexData %*% object$outer_loadings[exMVs, exLVs, drop=FALSE]
  exLatent <- matrix(data = numeric(), nrow = n, ncol = length(exLVs),
                     dimnames = list(seq_len(n), exLVs))
  exLatentp <- matrix(data = numeric(), nrow = n, ncol = length(exLVs),
                     dimnames = list(seq_len(n), exLVs))
  endLatent <- matrix(data = numeric(), nrow = n, ncol = length(endLVs),
                      dimnames = list(seq_len(n), endLVs))
  endLatentp <- matrix(data = numeric(), nrow = n, ncol = length(endLVs),
                      dimnames = list(seq_len(n), endLVs))
  
  exMVdata <- matrix(data = numeric(), nrow = n, ncol = length(exMVs),
                     dimnames = list(seq_len(n), exMVs))
  endMVdata <- matrix(data = numeric(), nrow = n, ncol = length(endMVs),
                     dimnames = list(seq_len(n), endMVs))

  # created from the proxies
  dataMV2 <- cbind(exMVdata, endMVdata) 
  
  for(LV in exLVs){
    
    if(attr(blocks[[LV]], "mode") == "A" || length(blocks[[LV]]) == 1){
      exLatent[, LV] <- scale(rnorm(n))
      
      if(length(blocks[[LV]]) == 1){
        exMVdata[, blocks[[LV]]] <- exLatent[, LV]
        next
      }

      exMVdata[, blocks[[LV]]] <- apply(tcrossprod(exLatent[, LV], L[blocks[[LV]], LV]),
                                        2, addEps, n = n)
    }
    
    if(attr(blocks[[LV]], "mode") == "B" && length(blocks[[LV]]) != 1){
      exMVdata[, blocks[[LV]]] <- scale(rmvnorm(n, mean =
        rep(0, length(blocks[[LV]])), sigma = S[[LV]]))

      exLatent[, LV] <- exMVdata[, blocks[[LV]]] %*% W[blocks[[LV]], LV] 
    }

    # Now create the proxy
    l <- L[blocks[[LV]], LV, drop=FALSE]
    llt <- tcrossprod(l)
    Sii <- cov(exMVdata[, blocks[[LV]]])
    exLatentp[, LV] <- exMVdata[, blocks[[LV]]] %*% l
    exLatentp[, LV] <- exLatentp[, LV] / sqrt(sum(llt * Sii))
  }


  # create endogenous data
  Latent <- cbind(exLatent, endLatent)
  Latentp <- cbind(exLatentp, endLatentp)
  predList <- predecessors(model)
  succList <- successors(model)

  for(LV in endLVs){
    # create the next LV  
    tmp <- Latent[, predList[[LV]]] %*%
      B[predList[[LV]], LV, drop=FALSE]
    Latent[, LV] <- apply(tmp, 2, addEps, n=n)
    if(length(blocks[[LV]]) == 1){
      endMVdata[, blocks[[LV]]] <- Latent[, LV]
      Latentp[, LV] <- scale(Latent[, LV])
      #if(LV == "Complaints") browser()
      next
    }
    
    endMVdata[, blocks[[LV]]] <- apply(tcrossprod(Latent[, LV], L[blocks[[LV]], LV]),
                                        2, addEps, n = n)
    
    
    ## tmp <- Latentp[, predList[[LV]]] %*%
    ##   B[predList[[LV]], LV, drop=FALSE]
    ## Latentp[, LV] <- tmp #apply(tmp, 2, addEps, n=n)
    ## if(length(blocks[[LV]]) == 1){
    ##   endMVdata[, blocks[[LV]]] <- Latent[, LV]
    ##   next
    ## } 

    ## dataMV2[, blocks[[LV]]] <- apply(tcrossprod(Latentp[, LV], L[blocks[[LV]], LV]),
    ##                                     2, addEps, n = n)

 

    ### Now, create the proxy
    # loadings from one block
    l <- L[blocks[[LV]], LV, drop=FALSE]
    llt <- tcrossprod(l) 
    Sii <- cov(endMVdata[, blocks[[LV]]])
    Latentp[, LV] <- endMVdata[, blocks[[LV]]] %*% l
    Latentp[, LV] <- Latentp[, LV] / sqrt(sum(llt * Sii))
  }

  ### Standard
  simData <- as.data.frame(cbind(exMVdata, endMVdata))

  ### Special
  dataMV2 <- apply(tcrossprod(Latentp, L), 2, addEps, n)
  
  result <- list(dataMV=simData, dataLV=Latent,
                 proxyLV = Latentp, dataMV2 = dataMV2)
  return(result)
}

## function to compute signal to noise ratio from R squared
## (determination coefficient)
snr <- function(rSquared) sqrt(rSquared/(1-rSquared))


## function for adding error
addEps <- function(x, n){
  if(var(x) > 1) stop("MVs variance exceeds 1.\nIllegal parameter choices.")
  sigma <- sqrt(1 - var(x))
  x <- x + rnorm(n=n, mean=0, sd=sigma)
  return(x)
}

addEps2 <- function(x, n){
  if(sd(x) > 1) stop("MVs variance exceeds 1.\nIllegal parameter choices.")
  sigma <- 1 - sd(x)
  x <- x + rnorm(n=n, mean=0, sd=sigma)
  return(x)
}

eps <- function(p, n){
  if(p > 1 || p <= 0) stop("Illegal parameter choice.")
  sigma <- sqrt(1 - p^2)
  cat("sigma: ", sigma, "\n", sep="")
  eps <- rnorm(n=n, mean=0, sd=sigma)
  return(eps)
}

## rescale() as inverse function of scale()
rescale <- function(data, newdata){
  if(!missing(newdata) && !all(colnames(data) %in% colnames(newdata))){
    stop("MVs must be available from newdata.")
  }
  if(is.null(attr(data, "scaled:center"))){
    message("No need to recenter, data is not centered.")
    m <- 0
  }
  else{
    m <- attr(data, "scaled:center")
  }
  if(is.null(attr(data, "scaled:scale"))){
    message("No need to rescale, data is not scaled.")
    s <- 1
  }
  else{
    s <- attr(data, "scaled:scale")
  }
  if(missing(newdata)){
    t(apply(data, 1, function(x) {x * s + m}))
  }
  else{
    newdata <- newdata[, colnames(data)]
    t(apply(newdata, 1, function(x) {x * s + m}))
  }
}

