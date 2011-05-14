# 12.05.2011
plsSimulator <- function(object, n, ordinal=TRUE, method=c("pearson", "kendall", "spearman"),
                         pairwise=FALSE, scale=c("scaled", "original"), total=FALSE, addError=FALSE)
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

  #exLatent <- scale(simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE])
  exLatent <- simexData %*% object$outer_weights[exMVs, exLVs, drop=FALSE]
  
  # adding error
  if(addError){
    addEps <- function(x, n){
      sigma <- sd(x)
      if(!is.null(attr(x, "R-squared"))){
        sigma <- sigma / (1-attr(x, "R-squared"))
      }
      if(!is.null(attr(x, "loadings"))){
        sigma <- sigma / (1-attr(x, "loadings"))
      }
      else sigma <- sigma / (1-0.7)
      x <- x + rnorm(n=n, mean=0, sd=sigma)
      return(x)
    }
  }
 
  
  # create endogen LVs from the model
  if(total){
    endLatent <- as.data.frame(exLatent %*% object$total_effects[exLVs,endLVs, drop=FALSE])
  }
  else {
    Latent <- exLatent
    predList <- predecessors(model)
    for(i in endLVs){
      tmp <- Latent[, predList[[i]]] %*% object$path_coefficients[predList[[i]], i , drop=FALSE]
      Latent <- cbind(Latent, tmp)
    }
    endLatent <- as.data.frame(Latent[, endLVs])
  }
      
  #levels <- colSums(model$D[, endLVs, drop=FALSE])
  if(addError){
    rSq <- rSquared(object)
    for(i in endLVs){
    #attr(endLatent[[i]], "level") <- levels[i]
      attr(endLatent[[i]], "R-squared") <- rSq[i,]
    }
    endLatent <- apply(endLatent, 2, addEps, n=n)
  }
  
  # create endogen MVs from the model     
  simendData <- as.matrix(endLatent) %*%
                t(object$outer_loadings[endMVs, endLVs])
  simendData <- as.data.frame(simendData)

  if(addError){
    loadings <- object$outer_loadings[endMVs,]
    loadings <- loadings[loadings!=0]
    names(loadings) <- endMVs
    for(i in endMVs){
      attr(simendData[[i]], "loadings") <- loadings[i]
    }
    simendData <- scale(apply(simendData, 2, addEps, n=n))
  }
  #simendData <- scale(apply(simendData, 2, addEps, n=n))
  simendData <- scale(apply(simendData, 2, addEps, n=n))
  
  simData <- cbind(simexData, simendData)
  if(scale=="original"){simData <- rescale(data, simData)}
  result <- simData                      
  return(result)
}
