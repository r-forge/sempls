# specific print methods

print.sempls <- function(x, what=c("coefficients", "loadings", "weights",
                         "path_coefficients", "total_effects"),
                         type=c("outer", "cross", "discriminant"),
                         cutoff=NULL, reldiff=0, na.print=".", ...)
{
  what <- match.arg(what)
  if(what=="coefficients"){
    print(x$coefficients, na.print=na.print, ...)
    invisible(x)
  }
  else if(what=="loadings"){
    loadings(x, type, cutoff, reldiff, na.print, ...)
  }
  else if(what=="weights"){
    weights(x, na.print, ...)
  }
  else if(what=="path_coefficients"){
    path_coefficients(x, na.print, ...)
  }
  else if(what=="total_effects"){
    total_effects(x, na.print, ...)
  }
  else{
    stop(paste("The argument 'what' must be an element of the vector",
               "c('coefficients', 'loadings', 'weights', ",
               "'path_coefficients', 'total_effects').", sep=""))
  }
}


loadings <- function(x, type=c("outer", "cross", "discriminant"),
                            cutoff=NULL, reldiff=0, na.print=".", ...)
{
  type <- match.arg(type)
  if(type=="outer"){
    outer <- x$outer_loadings
    outer[outer==0] <- NA
    if(!is.null(cutoff)){
      outer[outer < cutoff] <- NA
      print.table(x=outer, na.print=na.print, ...)
      invisible(outer)
    }
    else{
      print.table(x=outer, na.print=na.print, ...)
      invisible(outer)
    }
  }
  if(type=="cross"){
    cross <- x$cross_loadings
    cross[cross==0] <- NA
    if(!is.null(cutoff)) cross[cross < cutoff] <- NA
    print.table(cross, na.print=na.print, ...)
    invisible(cross)
  }
  # to check discriminant validity
  if(type=="discriminant"){
    if(reldiff < 0 | reldiff > 1)
      stop("Argument 'reldiff' only accepts values in the intervall [0,1].") 
    outer <- x$outer_loadings
    outer[outer==0] <- NA
    maxv <- apply(outer, 1, max, na.rm=TRUE)
    cross <- x$cross_loadings
    if(!is.null(cutoff)) cross[cross < cutoff] <- NA
    mind <- cross <= (maxv - reldiff * maxv)
    cross[mind] <- NA
    print.table(cross, na.print=na.print, ...)
    invisible(cross)
  }
}



weights <- function(x, na.print=".", ...){
  weights <- x$outer_weights
  weights[weights==0] <- NA
  print.table(weights, na.print=na.print, ...)
  invisible(weights)
}
  

path_coefficients <- function(x, na.print=".", ...){
  coeffs <- x$path_coefficients
  coeffs[coeffs==0] <- NA
  print.table(coeffs, na.print=na.print, ...)
  invisible(coeffs)
}


total_effects <- function(x, na.print=".", ...){
  coeffs <- x$total_effects
  coeffs[coeffs==0] <- NA
  print.table(coeffs, na.print=na.print, ...)
  invisible(coeffs)
}
