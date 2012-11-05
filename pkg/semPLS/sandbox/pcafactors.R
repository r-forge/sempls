### pcafactors
pcafactors <- function(data=NULL, R, tol=1e-7){
  if(missing(R)) R <- cor(data)

  V <- matrix(0, nrow=nrow(R), ncol=ncol(R))
  #diag(V) <- 1 - 1/diag(solve(R))
  R <- R - V
  iter <- 0
  while(TRUE){
    Valt <- V
    L <- svd(-R, nu=0)$v
    U <- R - crossprod(t(L[,1]))
    diag(V) <- diag(U)
    iter <- iter + 1
    if(norm(V - Valt, "F") < tol){
      cat("Iterations: ", iter, "\n", sep="")
      break
    }
  }
  l <- structure(L[,1], names=dimnames(data)[[2]]) 
  return(l)
}

pcafactors2 <- function(data=NULL, R, tol=1e-7){
  if(missing(R)) R <- cor(data)

  V <- matrix(0, nrow=nrow(R), ncol=ncol(R))
  #diag(V) <- 1 - 1/diag(solve(R))
  R <- R - V
  iter <- 0
  while(TRUE){
    Valt <- V
    pca <- prcomp(data)
    data <- pca$rotation[,1] %*% t(pca$x[, 1])
    U <- R - crossprod(t(L[,1]))
    diag(V) <- diag(U)
    iter <- iter + 1
    if(norm(V - Valt, "F") < tol){
      cat("Iterations: ", iter, "\n", sep="")
      break
    }
  }
  l <- structure(L[,1], names=dimnames(data)[[2]]) 
  return(l)
}
