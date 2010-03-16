step5 <-
function(Wold, Wnew, tol=1e-7, converged){
  relDiff <- abs((Wold-Wnew)/Wnew)
  if (all(relDiff[!is.nan(relDiff)] < tol)) converged <- TRUE
  else Wold <- Wnew
  return(list(Wold=Wold, converged=converged))
}
