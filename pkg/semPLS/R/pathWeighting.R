# path weighting scheme
pathWeighting <-
function(model, fscores, pairwise, method){
  method <- "pearson" ## for other methods: convergence problems!
  ifelse(pairwise, use <- "pairwise.complete.obs",
                   use <- "everything")
  D <- model$D
  latent <- model$latent
  E <- D - t(D)
  pred <- vector(mode="list", length=ncol(E))
  
  for (i in 1:ncol(E)){
    oneLatent <- colnames(E)[i]
    pred[[i]] <- predecessors(E, oneLatent)
  }
  names(pred) <- colnames(E)

  # calculating the inner weights
  innerW <- E
  for (i in 1:ncol(E)){
    if(all(!is.na(pred[[i]]))){
      innerW[pred[[i]],i] <- solve(cor(as.matrix(fscores[,pred[[i]]])
                                       , use=use, method=method)) %*%
                             cor(fscores[,pred[[i]]], fscores[,i], use=use, method=method)
    }
  }

  innerW[E == 0] <- 0
  innerW[E == -1] <- cor(as.matrix(fscores[, latent]), use=use, method=method)[E == -1]
  # return the matrix of inner weights  
  return(innerW)
}
