# 10.05.2011
predict <- function(object, newdata, what=c("LVs", "MVs"), scale=c("scaled", "original")){
    # total: only for MVs. Include exogenous and endogenous MVs
    # Note: All MVs are treated as if they were reflective.
    what <- match.arg(what)
    scale <- match.arg(scale)
    model <- object$model
    data <- object$data
    exLVs <- exogen(model)
    exMVs <- unlist(model$blocks[exLVs])
    factor_scores <- object$factor_scores
    if(!missing(newdata)){
      if(!all(exMVs %in% colnames(newdata))){
        stop("The MVs related to exogenous LVs must be contained in newdata!")
      }
      else {
        data <- scale(newdata)[,exMVs]
        attr(data, "scaled:center") <- attr(object$data, "scaled:center")
        attr(data, "scaled:scale") <- attr(object$data, "scaled:center")
        factor_scores <- data[, exMVs, drop=FALSE] %*%
                         object$outer_weights[exMVs, exLVs, drop=FALSE]
      }       
    }
    # missing factor scores?
    fsMissing <- !complete.cases(factor_scores[, exLVs, drop=FALSE])
    # mising MV data?
    mvMissing <- !complete.cases(data[, exMVs, drop=FALSE])

    # situation A: complete data
    # LVs
    if(what=="LVs"){
      Y_hat <- matrix(NA, nrow=nrow(object$factor_scores), ncol=ncol(object$factor_scores))
      # Only exogenous LV can be used to predict 
      Y_hat[!fsMissing,] <- factor_scores[!fsMissing, exLVs, drop=FALSE] %*%
                            object$total_effects[exLVs,, drop=FALSE]
      return(Y_hat)
    }

    # MVs
    if(what=="MVs"){
      mv_hat <- matrix(NA, nrow=nrow(object$data), ncol=ncol(object$data))
      colnames(mv_hat) <- colnames(object$data)
      # mv_prediction      
      mv_hat[!mvMissing,] <- factor_scores[!fsMissing, exLVs, drop=FALSE] %*%
                             object$total_effects[exLVs,, drop=FALSE] %*%
                             t(object$outer_loadings)
      if(scale=="original"){mv_hat <- rescale(data, mv_hat)}
      result <- mv_hat                       
      return(result)
    }
}

# rescales a data matrix, which has been scaled before using 'scale()'.
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

# mean square error of prediction
msep <- function(object, mvPrediction, mvObserved, exMVs){
  sum((mvPrediction - mvObserved)^2, na.rm=TRUE)/
    (prod(dim(mvPrediction)) - nrow(object$coeff) - sum(is.na(mvPrediction)))
}
