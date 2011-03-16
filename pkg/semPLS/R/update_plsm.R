# Method to update a PLS path model of class 'plsm'
plsmEdit <- function(model, data)
{
  sm <- model$strucmod
  cat("Edit the structural model!")
  sm <- edit(sm, title="Edit the structural model!")
  cat(" Done.\n")
  cat("Edit the measurement model!")
  mm <- edit(model$measuremod, title="Edit the measurement model!")
  cat(" Done.\n")
  model <- plsm(data, strucmod=sm, measuremod=mm, order=model$order)
  return(model)
}

addPath <- function(model, from=character(), to=character()){
  if(!c(from,to) %in% model$latent){
    stop("LVs without indicators.")
  }
  sm <- model$strucmod
  sm <- rbind(sm, cbind(from, to))
  dummy <- as.data.frame(matrix(NA, nrow=1, ncol=length(model$manifest)))
  attr(dummy, "names") <- model$manifest
  mm <- model$measuremod
  model <- plsm(data=dummy, strucmod=sm, measuremod=mm, order=model$order)
  return(model)
}

removePath <- function(model, from=character(), to=character()){
  sm <- model$strucmod
  ind <- which(sm[,1] %in% from & sm[,2] %in% to)
  if(length(ind)==0) stop("Path not in model.")
  else sm <- sm[-ind,]
  dummy <- as.data.frame(matrix(NA, nrow=1, ncol=length(model$manifest)))
  attr(dummy, "names") <- model$manifest
  mm <- model$measuremod
  model <- plsm(data=dummy, strucmod=sm, measuremod=mm, order=model$order)
  return(model)
}

addIndicator <- function(model, data, LV=character(), MVs=character()){
  if(!LV %in% model$latent){
    stop(paste("You can not add indicators to non existent LV '", LV, "'!\n",
               "Try to use the method 'addLV'.", sep=""))
  }
  mm <- model$measuremod
  block <- model
}

removeIndicator <- function(model){}

invertLV <- function(model){}

addLV <- function(model, data,  LV=character(), MVs=character(), pred=character(), succ=character())
{
  sm <- model$strucmod
  mm <- model$measuremod
}

removeLV <- function(model, which){
  sm <- model$strucmod
  ind1 <- which(sm[,1] %in% which)
  ind2 <- which(sm[,2] %in% which)
  ind <- unique(c(ind1, ind2))
  # remove LVs in structural model
  smv1 <- unique(sm[1:(2*nrow(sm))])
  sm <- sm[-ind,]
  smv2 <- unique(sm[1:(2*nrow(sm))])
  lost <- setdiff(smv1, c(smv2,which))
  if(length(lost) > 0){
    warning(paste("Lost variables: ",
                  paste(lost, collapse=", "),
                  ".\n", sep=""))
    which <- c(which, lost)
  }

  mm <- model$measuremod
  ind1 <- which(mm[,1] %in% which)
  ind2 <- which(mm[,2] %in% which)
  ind <- unique(c(ind1, ind2))
  # remove LVs in measurement model
  mm <- mm[-ind, ]
  dummy <- as.data.frame(matrix(NA, nrow=1, ncol=length(model$manifest)))
  attr(dummy, "names") <- model$manifest
  model <- plsm(data=dummy, strucmod=sm, measuremod=mm, order=model$order)
  return(model)
}

