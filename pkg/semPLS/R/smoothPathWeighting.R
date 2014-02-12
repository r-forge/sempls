### Smooth path weighting scheme Armin Monecke 2013-10-17
smoothPathWeighting <-
function(model, fscores, pairwise, method, ...){
  ## if(pairwise) stop("smoothPathWeighting can not be used with option pairwise")
  method <- "pearson" ## for other methods: convergence problems!
  ifelse(pairwise, use <- "pairwise.complete.obs",
                   use <- "everything")

  latent <- model$latent
  pred <- predecessors(model)
  succ <- successors(model)
  ## calculating the inner gams and put them in a named list
  innerW <- vector(mode = "list", length = length(latent))
  names(innerW) <- latent
  fscores <- as.data.frame(fscores)
  for (i in latent){
    if(length(pred[[i]])==0){
        innerW[[i]] <- 0
        next
    }
    ## FixMe: needs fix for resempls()! [done]
    rhs <- paste(sprintf('s(%s , bs = "ps", k = %s)',
                        c(pred[[i]]), #succ[[i]]),
                        list(...)[[1]]$k),
                 collapse = " + ")
    
    f <- as.formula(sprintf("%s ~ %s", i, rhs))
    
    thisGAM <- gam(f, data = fscores)
    innerW[[i]] <- predict(thisGAM)
  }

  latent <- model$latent
  fscores_matrix <- as.matrix(fscores[, latent])
  E <- t(model$D)
  E[E == 1] <- cor(fscores_matrix, use=use, method=method)[E == 1]

  innerW2 <- fscores_matrix %*% E

  innerW <- as.data.frame(innerW) + innerW2
  
  ## return the scaled predictions instead of inner weights
  return(innerW)
}

gamFUN <- function(model, fscores, smoothControl, ...){
  endo <- endogenous(model)
  pred <- predecessors(model)
  succ <- successors(model)
  ## calculating the inner gams and put them in a named list
  gamList <- vector(mode = "list", length = length(endo))
  names(gamList) <- endo
  fscores <- as.data.frame(fscores)
  for (i in endo){
    rhs <- paste(sprintf('s(%s , bs = "ps", k = %s)',
                        pred[[i]],
                        smoothControl$k),
                 collapse = " + ")
    
    f <- as.formula(sprintf("%s ~ %s", i, rhs))
    
    gamList[[i]] <- gam(f, data = fscores)
  }
  ## return named list with GAMs
  return(gamList)
}
