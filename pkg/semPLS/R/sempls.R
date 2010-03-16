# Estimates factor scores and parameters for PLS path models 
sempls <- function(model, ...){
  UseMethod("sempls", model)
}

sempls.plsm <-
function(model, data, maxit=100, tol=1e-7, scaled=TRUE, sum1=TRUE, E="A", pairwise=FALSE,
         method=c("pearson", "kendall", "spearman"), ...){
  method <- match.arg(method)
  result <- list(coefficients=NULL, path_coefficients=NULL,
                 outer_loadings=NULL ,cross_loadings=NULL,
                 total_effects=NULL,inner_weights=NULL, outer_weights=NULL,
                 blocks=NULL, factor_scores=NULL, data=NULL, scaled=scaled,
                 model=model, weighting_scheme=NULL, sum1=sum1, pairwise=pairwise,
                 method=method, iterations=NULL,
                 tolerance=tol, maxit=maxit, N=NULL, incomplete=NULL)
  class(result) <- "sempls"
  
  # checking the data
  data <- data[, model$manifest]
  N <- nrow(data)
  missings <- which(complete.cases(data)==FALSE)
  if(length(missings)==0){
    cat("All", N ,"observations are valid.\n")
    if(pairwise){
      pairwise <- FALSE
      cat("Argument 'pairwise' is reset to FALSE.\n")
    }
  }
  else if(length(missings)!=0 & !pairwise){
    # Just keeping the observations, that are complete.
    data <- na.omit(data[, model$manifest])
    cat("Data rows:", paste(missings, collapse=", "),
        "\nare not taken into acount, due to missings in the manifest variables.\n",
        "Total number of complete cases:", N-length(missings), "\n")
  }
  else{
     cat("Data rows", paste(missings, collapse=", "),
         " contain missing values.\n",
         "Total number of complete cases:", N-length(missings), "\n")
  }
  ## check the variances of the data
  if(!all(apply(data, 2, sd, na.rm=TRUE) != 0)){
     stop("The MVs: ",
          paste(colnames(data)[which(apply(data, 2, sd)==0)], collapse=", "),
          "\n  have standard deviation equal to 0.\n",
          "  Recheck model!\n")
  }

  ## scale data?
  # Note: scale() changes class(data) to 'matrix'
  if(scaled) data <- scale(data)

  #############################################
  # step 1: Initialisation
  stp1 <- step1(model, data, sum1=sum1, pairwise)
  factor_scores <- stp1$latent
  Wold <- stp1$outerW

  #############################################
  # Select the weighting scheme
  if(E=="A") {
    innerWe <- centroid
    result$weighting_scheme <- "centroid"
  }
  if(E=="B") {
    innerWe <- factorial
    result$weighting_scheme <- "factorial"
  }
  if(E=="C") {
    innerWe <- pathWeighting
    result$weighting_scheme <- "path weighting"
  }
  
  #######################################################################
  # Iterate over step 2 to step 5
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
    if(i == maxit && !converged){
      cat(paste("No convergence after ", i, " iterations.\n", sep=""))      
      break
    }
    ############################################# 
    i <- i+1
  }
  ########################################################################
  
  if(converged){
      cat(paste("Converged after ", (i-1), " iterations.\n",
                "Tolerance: ", tol ,"\n", sep=""))
      if (E=="A") cat("Scheme: centroid\n")
      if (E=="B") cat("Scheme: factorial\n")
      if (E=="C") cat("Scheme: path weighting\n")
  }

  
  ifelse(pairwise, use <- "pairwise.complete.obs", use <- "everything")
  result$path_coefficients <- pathCoeff(model=model, factor_scores, method, pairwise)
  result$cross_loadings <- cor(data, factor_scores, use, method)
  result$outer_loadings <- result$cross_loadings
  result$outer_loadings[Wnew==0] <- 0
  result$total_effects <- totalEffects(result$path_coefficients)
  result$inner_weights <- innerW
  result$outer_weights <- Wnew
  result$factor_scores <- factor_scores
  result$data <- data
  result$N <- N
  result$incomplete <- missings
  result$iterations <- (i-1)
  result$coefficients <- coefficients(result)
  return(result)
}

print.sempls <- function(x, ...){
  print(x$coefficients)
  invisible(x)
}
