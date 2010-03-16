# used in 'pathWeighting'
predecessors <- function(E, oneLatent){  
    if(all(E[, oneLatent] != 1)){
      #cat(paste(oneLatent, "is an endogenous variable.\n"))
      pred <- NA
      return(pred)
    } 
    else{
      i <- which(oneLatent == rownames(E))
      pred <- rownames(E)[E[, i] == 1]
      E <- E[-i, -i]
    }
    while(!is.null(pred) && pred %in% colnames(E)){     
      if(all(E[, pred] != 1)){
        return(pred)
      }
      else{
        index <- which(colnames(E) %in% pred)
        for (i in index){
          pred <- append(pred, rownames(E)[E[, i] == 1])
        }   
        E <- E[-index, -index]
      }
    }
    return(unique(pred))
}
