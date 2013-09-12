semspec2plsm <- function(object, ...){
  stopifnot(require("semspec"))
  args <- as_semPLS_syntax(object)
  do.call(plsm, args)
}

plsm2semrepr <- function(object){
  ## comment
  regression <- structure(as.data.frame(object$strucmod),
    names = c("lhs", "rhs"))
  regression <- cbind(type = "regression", regression,
    lhsparam = regression$lhs, rhsparam = regression$rhs, group = factor(NA),
    level = factor(NA), param = sprintf("%s_%s", regression$rhs, regression$lhs),
    free = TRUE)

  latent <- structure(as.data.frame(object$measuremod),
    names = c("lhs", "rhs"))
  latent <- cbind(type = "latent", latent,
    lhsparam = latent$lhs, rhsparam = latent$rhs, group = factor(NA),
    level = factor(NA), param = sprintf("%s_%s", latent$rhs, latent$lhs),
    free = TRUE)

  ret <- rbind(latent, regression)
  structure(ret, class = c("semrepr", "data.frame"))
}
 
plsm2semspec <- function(object){
  ## comments
  stopifnot(require("semspec"))
  latent <- mapply(function(block, LV){
        if(attr(block, "mode") == "A"){
          sprintf("%s ~ %s", paste(block, collapse = " + "), LV)
        } else {
          sprintf("%s ~ %s", LV, paste(block, collapse = " + "))
        }
      }, block=object$blocks, LV=names(object$blocks))
  latent <- sprintf("latent(%s)", latent)

  rhs <- apply(object$D, 1, function(X){
      rhs <- if(all(!X)) NULL else
        paste(names(X)[as.logical(X)], collapse = " + ")
      }
    )
  ind <- !sapply(rhs, is.null)
 lhs <- rownames(object$D)
 regression <- sprintf("regression(%s ~ %s)", lhs[ind], rhs[ind])

 parse(text = paste(c(regression, latent), collapse = " + "))
}
