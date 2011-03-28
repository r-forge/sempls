# specific print methods

# check discriminant validity
loadings <- function(object, ...){
  UseMethod("loadings", object)
}

loadings.sempls <- function(object, type=c("outer", "cross", "discriminant"), cutoff=NULL, tol=0, na.print=".", ...)
{
  type <- match.arg(type)
  if(type=="outer"){
    outer <- object$outer_loadings
    outer[outer==0] <- NA
    if(!is.null(cutoff)){
      outer[outer < cutoff] <- NA
      print.table(x=outer, na.print=na.print, ...)
    }
    else print.table(x=outer, na.print=na.print, ...)
  }
  if(type=="cross"){
    cross <- object$cross_loadings
    cross[cross==0] <- NA
    if(!is.null(cutoff)) cross[cross < cutoff] <- NA
    print.table(cross, na.print=na.print, ...)
  }
  if(type=="discriminant"){
    if(tol < 0 | tol > 1)
      stop("Argument 'tol' only accepts values in the intervall [0,1].") 
    outer <- object$outer_loadings
    outer[outer==0] <- NA
    maxv <- apply(outer, 1, max, na.rm=TRUE)
    cross <- object$cross_loadings
    if(!is.null(cutoff)) cross[cross < cutoff] <- NA
    mind <- cross < (maxv - tol*maxv)
    cross[mind] <- NA
    print.table(cross, na.print=na.print, ...)
  }
}

weights <- function(object, ...){
  UseMethod("weights", object)
}

weights.sempls <- function(object, na.print=".", ...){
  weights <- object$outer_weights
  weights[weights==0] <- NA
  print.table(weights, na.print=na.print, ...)
}
  
path_coefficients <- function(object, ...){
  UseMethod("path_coefficients", object)
}

path_coefficients.sempls <- function(object, na.print=".", ...){
  coeffs <- object$path_coefficients
  coeffs[coeffs==0] <- NA
  print.table(coeffs, na.print=na.print, ...)
}

total_effects <- function(object, ...){
  UseMethod("total_effects", object)
}

total_effects.sempls <- function(object, na.print=".", ...){
  coeffs <- object$total_effects
  coeffs[coeffs==0] <- NA
  print.table(coeffs, na.print=na.print, ...)
}
