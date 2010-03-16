# Refitting the sempls object for the boot.sempls method.
resempls <-
function(sempls, data, start=c("ones", "old"), method, ...){
  start <- match.arg(start)
  bootMethod <- method
  sum1 <- sempls$sum1
  tol <- sempls$tolerance
  maxit <- sempls$maxit
  model <- sempls$model
  pairwise <- sempls$pairwise
  method <- sempls$method
  
  ## scale data?
  # Note: scale() changes class(data) to 'matrix'
  if(sempls$scaled) data <- scale(data)

  #############################################
  # step 1
  # initialization with results from fitted model
  if(start=="old"){
    factor_scores <- sempls$factor_scores
    Wold <- sempls$outer_weights
  }
  else if(start=="ones"){
    # Weights not adding up to 1 (14.08.2009)
    stp1 <- step1(model, data, sum1=FALSE, pairwise)
    factor_scores <- stp1$latent
    Wold <- stp1$outerW
  }

  #############################################
  # weighting scheme
  innerWe <- eval(parse(text=sub(" w", "W", sempls$weighting_scheme)))
  

  #############################################
  # Iterate over step 2 to 5
  
  i <- 1
  converged <- FALSE
  while(!converged){
    
    #############################################    
    # step 2
    innerW <- innerWe(model, fscores=factor_scores, pairwise, method)
    factor_scores <- step2(Latent=factor_scores, innerW, blocks=model$blocks, pairwise)

    #############################################
    # step 3
    Wnew <-  outerApprx(Latent=factor_scores, data, blocks=model$blocks,
                        sum1=sum1, pairwise, method)
    
    #############################################    
    # step 4
    factor_scores <- step4(data, outerW=Wnew, blocks=model$blocks, pairwise)
    
    #############################################    
    # step 5
    st5 <- step5(Wold, Wnew, tol, converged)
    Wold <- st5$Wold
    converged <- st5$converged

  # try-error?  
    if(i == maxit && !converged){
      class(result) <- c(class(result), "try-error")
      break
    }
    
    i <- i+1
  }
  #############################################
  
  result <- sempls
  
  ### bootstrap method ##################################################
  # Construct level changes
  clcIndex <- NULL
  if(bootMethod=="ConstructLevelChanges"){
    result$cross_loadings <- cor(data, factor_scores)
    result$outer_loadings <- result$cross_loadings
    result$outer_loadings[Wnew==0] <- 0
    clcIndex <- which(abs(colSums(sempls$outer_loadings - result$outer_loadings)) >
                    abs(colSums(sempls$outer_loadings + result$outer_loadings)))
    # change the signs of the weights
    Wnew[, clcIndex] <- ifelse(sum1, -apply(as.matrix(Wnew[, clcIndex]), 2, sum1),
                                     -Wnew[, clcIndex])
    # repeat step4
    factor_scores <- step4(data, outerW=Wnew, blocks=model$blocks, pairwise)
  }
  #######################################################################
  
  result$cross_loadings <- cor(data, factor_scores)
  result$outer_loadings <- result$cross_loadings
  result$outer_loadings[Wnew==0] <- 0
  result$path_coefficients <- pathCoeff(model=model, factor_scores, method, pairwise)
  result$total_effects <- totalEffects(result$path_coefficients)
  result$inner_weights <- innerW
  result$outer_weights <- Wnew
  result$factor_scores <- factor_scores
  result$data <- data
  result$iterations <- (i-1)
  result$coefficients <- coefficients(result)
  result$clcIndex <- clcIndex
  return(result)
}
