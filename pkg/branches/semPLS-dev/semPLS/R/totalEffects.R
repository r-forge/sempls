totalEffects <- function(object){
  UseMethod("totalEffects", object)
}

# Calculates the total effects
totalEffects.default <- function(pathCoeff){
  ret <- pathCoeff
  step <- pathCoeff
  for (i in 2:ncol(pathCoeff)){
    step <- step %*% pathCoeff
    ret <- step + ret
  }
  return(ret)
}

totalEffects.sempls <- function(object){
  coeffs <- object$total_effects
  class(coeffs) <- "totalEffects"
  return(coeffs)
}

print.totalEffects <- function(x, na.print=".", digits=2, ...){
  coeffs <- x
  coeffs[coeffs==0] <- NA
  print.table(coeffs, na.print=na.print, digits=digits, ...)
  invisible(x)
}
  
