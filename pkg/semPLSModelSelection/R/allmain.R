### Armin (first: 2012-09-21; Last: 2013-01-23)
### changes: 'saturate' -> 'allmain' (2013-01-23)

### get the model including all main effects (with respect to an LV in focus)
allmain <- function(model, LV, ...)
  UseMethod("allmain", model)

allmain.plsm <- function(model, LV, ...){
  LVs <- model$latent
  Dn <- semPLS:::reorder(model$D)$Dn[, LV]
  allpred <- names(Dn)[Dn > 1]
  model <- semPLS:::addPath.plsm(model, from = allpred, to = LV)
  return(model)
}

### fit the model including all main effects (with respect to an LV in focus)
fitallmain <- function(object, LV, ...)
  UseMethod("fitallmain", object)

fitallmain.sempls <- function(object, LV, ...){
  model_sat <- allmain(model = object$model, LV = LV)
  object$model <- model_sat
  objectFull <- semPLS:::resempls(object, data = object$data,
    method = "Standard")
  return(objectFull)
}
