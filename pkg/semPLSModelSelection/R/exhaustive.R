### Armin (first: 2012-09-21; Last: 2013-01-23)
### changes: 'saturate' -> 'allmain' (2013-01-23)

### get the model including all main effects (with respect to an LV in focus)
exhaustive <- function(model, LV, ...)
  UseMethod("exhaustive", model)

exhaustive.plsm <- function(model, LV, ...){
  LVs <- model$latent
  Dn <- semPLS:::reorder(model$D)$Dn[, LV]
  allpred <- names(Dn)[Dn > 0]
  model <- removePath(model, from = allpred, to = LV)
  p <- length(allpred)
  model_list <- vector("list", length = 2^p - 1)
  names_vec <- vector("character", length = 2^p - 1)
  counter <- 1
  for(size in seq_len(p)){
    pred_list <- combn(allpred, size, simplify = FALSE)
    for(pred in pred_list){
      model_list[[counter]] <-
        semPLS:::addPath.plsm(model, from = pred, to = LV)
       names_vec[counter] <- paste(pred, collapse = " + ")
      counter <- counter + 1
    }
  }
  names(model_list) <- names_vec
  return(model_list)
}



### fit the model including all main effects (with respect to an LV in focus)
fitexhaustive <- function(object, LV, ...)
  UseMethod("fitexhaustive", object)

fitexhaustive.sempls <- function(object, LV, ...){
  pred_orig <- paste(predecessors(model = object$model)[[LV]],
    collapse = " + ")
  all_models <- exhaustive(model = object$model, LV = LV)
  all_fits <- vector("list", length = length(all_models))
  names(all_fits) <- names(all_models)
  all_fits[[pred_orig]] <- object
  pred_other <- setdiff(names(all_fits), pred_orig)
  for(pred in pred_other){
    object$model <- all_models[[pred]]
    all_fits[[pred]] <- semPLS:::resempls(object, data = object$data,
      method = "Standard")
  }
  class(all_fits) <- "exhaustivesempls"
  return(all_fits)
}
