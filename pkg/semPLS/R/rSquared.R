# Method for accessing the R-squares for the submodels of a path model fitted by 'sempls'
rSquared <- function(object){
  UseMethod("rSquared")
}

rSquared.sempls <- function(object){
  Y_hat <- object$factor_scores %*% object$path_coefficients
  R_squared <- apply(Y_hat, 2, var) / apply(object$factor_scores, 2, var)
  R_squared[R_squared==0] <- NA
  R_squared <- as.matrix(R_squared)
  colnames(R_squared) <- "R-squared"
  return(R_squared)
}
  
