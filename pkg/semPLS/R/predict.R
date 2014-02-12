### last edit: Mi 12. Feb 15:20:07 CET 2014
### 10.05.2011
predict.sempls <-
    function(object,
             newdata,
             what = c("LVs", "MVs"),
             scale = c("scaled", "original"),
             total = FALSE)
{
    ## Note: All MVs are treated as if they were reflective.
    what <- match.arg(what)
    scale <- match.arg(scale)
    model <- object$model
    data <- object$data
    exLVs <- exogenous(model)
    exMVs <- unlist(model$blocks[exLVs])
    factor_scores <- object$factor_scores

    if(!missing(newdata)){
      if(!all(exMVs %in% colnames(newdata))  && total){
        stop("The MVs related to exogenous LVs must be contained in newdata!")
      }
      else {

        ## data <- scale(newdata)[,exMVs]
        if(!total){
          data <- scale2(data, newdata)
          active <- colnames(data)
          ## total <- TRUE
          ## message("Set Argument total to TRUE.")
          factor_scores <- data %*% object$outer_weights[active, , drop = FALSE]

        }
        ## attr(data, "scaled:center") <- attr(object$data, "scaled:center")
        ## attr(data, "scaled:scale") <- attr(object$data, "scaled:center")
        else{
          data <- scale2(data, newdata[,exMVs])
          factor_scores <- data[, exMVs, drop=FALSE] %*%
              object$outer_weights[exMVs, exLVs, drop=FALSE]
        }
      }
    }

    if(!total){
      ## missing factor scores?
      fsMissing <- !complete.cases(factor_scores)
      ## mising MV data?
      mvMissing <- !complete.cases(data)

    }
    else{
      ## missing factor scores?
      fsMissing <- !complete.cases(factor_scores[, exLVs, drop=FALSE])
      ## mising MV data?
      mvMissing <- !complete.cases(data[, exMVs, drop=FALSE])
    }

    ### LVs
    if(what=="LVs" & total){
      Y_hat <- matrix(NA, nrow=nrow(factor_scores), ncol=ncol(object$factor_scores))
      ## Only exogenous LVs can be used to predict
      Y_hat[!fsMissing,] <- factor_scores[!fsMissing, exLVs, drop=FALSE] %*%
                            object$total_effects[exLVs,, drop=FALSE]
      colnames(Y_hat) <- colnames(object$factor_scores)
      return(Y_hat)
    }

    if(what=="LVs" & !total){
      Y_hat <- matrix(NA, nrow=nrow(factor_scores), ncol=ncol(object$factor_scores))
      ## all LVs can be used to predict
      Y_hat[!fsMissing,] <- factor_scores[!fsMissing, , drop=FALSE] %*%
                            object$path_coefficients[, , drop=FALSE]
      colnames(Y_hat) <- colnames(object$factor_scores)
      return(Y_hat)
    }

    ### MVs
    if(what=="MVs" & total){
      mv_hat <- matrix(NA, nrow=nrow(data), ncol=ncol(object$data))
      colnames(mv_hat) <- colnames(object$data)
      ## mv_prediction
      mv_hat[!mvMissing,] <- factor_scores[!fsMissing, exLVs, drop=FALSE] %*%
                             object$total_effects[exLVs,, drop=FALSE] %*%
                             t(object$outer_loadings)
      ## if(scale=="original"){mv_hat <- rescale(data, mv_hat)}
      if(scale=="original"){mv_hat <- rescale(object$data, mv_hat)}
      result <- mv_hat
      return(result)
    }

    if(what=="MVs" & !total){
      ## mv_hat <- matrix(NA, nrow=nrow(object$data), ncol=ncol(object$data))
      mv_hat <- matrix(NA, nrow=nrow(data), ncol=ncol(object$data))
      colnames(mv_hat) <- colnames(object$data)
      ## mv_prediction
      mv_hat[!mvMissing,] <- factor_scores[!fsMissing, , drop=FALSE] %*%
                             ## object$path_coefficients[,, drop=FALSE] %*%
                             t(object$outer_loadings)
      ## if(scale=="original"){mv_hat <- rescale(data, mv_hat)}
      if(scale=="original"){mv_hat <- rescale(object$data, mv_hat)}
      result <- mv_hat
      return(result)
    }
}

scale2 <-
  function(data, newdata){
    active_set <- colnames(data) %in% colnames(newdata)
    newdata <- newdata[, colnames(data)[active_set]]
    if(!missing(newdata) && !all(colnames(data)[active_set] %in% colnames(newdata))){
      stop("MVs must be available from newdata.")
    }
    if(is.null(attr(data, "scaled:center"))){
      message("No need to recenter, data is not centered.")
      m <- 0
    }
    else{
      m <- attr(data, "scaled:center")[active_set]
    }
    if(is.null(attr(data, "scaled:scale"))){
      message("No need to rescale, data is not scaled.")
      s <- 1
    }
    else{
      s <- attr(data, "scaled:scale")[active_set]
    }
    newdata <- newdata[, colnames(data)[active_set]]
    newdata <- t(apply(newdata, 1, function(x) {(x - m) / s}))
    attr(newdata, "scaled:center") <- attr(data, "scaled:center")[active_set]
    attr(newdata, "scaled:scale") <- attr(data, "scaled:center")[active_set]
    return(newdata)
}

### rescales a data matrix, which has been scaled before using 'scale()'.
rescale <- function(data, newdata){
  if(!missing(newdata)){
    active_set <- colnames(data) %in% colnames(newdata)
  }
  if(!missing(newdata) && !all(colnames(data)[active_set] %in% colnames(newdata))){
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
    newdata <- newdata[, colnames(data)[active_set]]
    t(apply(newdata, 1, function(x) {x * s[active_set] + m[active_set]}))
  }
}

### mean square error of prediction
msep <- function(object, mvPrediction, mvObserved, exMVs){
  sum((mvPrediction - mvObserved[, colnames(mvPrediction)])^2, na.rm=TRUE)/
    (prod(dim(mvPrediction)) - nrow(object$coeff) - sum(is.na(mvPrediction)))
}
