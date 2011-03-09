# 01.03.2011
residuals <- function(object, what=c("LVs", "MVs"), scale=c("original", "scaled"), total=FALSE){
    what <- match.arg(what)
    scale <- match.arg(scale)
    model <- object$model
    data <- object$data
    # LVs
    if(what=="LVs"){
        res <- object$factor_scores - predict(object, what, scale, total)
    }
    # MVs
    else{
      if(scale=="scaled"){
        pdata <- predict(object, what, scale, total)$mv_prediction
        res <- data - pdata
      }
      else{
        m <- attr(data, "scaled:center")
        s <- attr(data, "scaled:scale")
        data <- t(t(data) * s) + m
        pdata <- predict(object, what, scale, total)$mv_prediction
        res <- data - pdata
      }
    }
    return(res)
}
