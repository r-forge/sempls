# Method to update a PLS path model of class 'plsm'
plsmEdit <- function(model, data, order=c("generic", "alphabetical"))
{
  order <- match.arg(order)
  cat("Edit the structural model!")
  sm <- edit(ECSImobi$strucmod, title="Edit the structural model!")
  cat(" Done.\n")
  cat("Edit the measurement model!")
  mm <- edit(ECSImobi$measuremod, title="Edit the measurement model!")
  cat(" Done.\n")
  model <- plsm(data, strucmod=sm, measuremod=mm, order=order)
}

addPath <- function(model){}

removePath <- function(model){
 stop("Chain is broken!")
}

addIndicator <- function(model, data){}

removeIndicator <- function(model){}

invertLV <- function(model){}

addLV <- function(model, data,  name=char(), mvs, pred, succ){
  sm <- model$strucmod
  mm <- model$measuremod
  
}

removeLV <- function(model, which){
  sm <- model$strucmod
  ind1 <- which(sm[,1] %in% which)
  ind2 <- which(sm[,2] %in% which)
  ind <- unique(c(ind1, ind2))
  mm <- model$measuremod
  pred <- predecessors(model)
  succ <- successors(model)
  for(i in model$latent){
    if(length(pred[[i]])==0 & length(succ[[i]])==0){
      stop("Chain is broken!")
    }
}
