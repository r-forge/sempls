meanModel <- function(modelList, which=1:length(modelList)){
  x <- modelList[which]
  meanModel <- x[[1]]
  n <- length(which)
  modelMean <- function(x, what){
    tmp <- NULL
    tmp <- x[[1]][[what]]
    for(i in 2:n){
      tmp <- tmp + x[[i]][[what]]
    }
    ret <- tmp/n
    return(ret)
  }
  meanModel$path_coefficients <- modelMean(x, "path_coefficients")
  meanModel$outer_loadings <- modelMean(x, "outer_loadings")
  meanModel$cross_loadings <- modelMean(x, "cross_loadings")
  meanModel$total_effects <- modelMean(x, "total_effects")
  meanModel$inner_weights <- modelMean(x, "inner_weights")
  meanModel$outer_weights <- modelMean(x, "outer_weights")
  meanModel$factor_scores <- modelMean(x, "factor_scores") * sqrt(n)
  meanModel$data <- modelMean(x, "data") * sqrt(n)
  #meanModel$weights_evolution <- modelMean(x, "weights_evolution")
  meanModel$iterations <- modelMean(x, "iterations")
  class(meanModel) <- c("meanModel", "sempls")
  meanModel$coefficients <- coef(meanModel)
  return(meanModel)
}
