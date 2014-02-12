qSquared <- function(object, ...){
  UseMethod("qSquared")
}

# d: ommision distance
# dlines: TRUE => leaving out the same observation for the MV-Blocks
# Note: if total=TRUE, total_effects are used, else the path_coefficients
qSquared.sempls <- function(object, d=NULL, impfun, dlines=TRUE, total=FALSE, ...){
    if(missing(impfun)){
      if(!object$pairwise & object$weighting_scheme != "smooth path weighting"){
        object$pairwise <- !object$pairwise
        message("Set 'pairwise' to TRUE.")
        impfun <- function(data) return(data)
      }
      else{
        impfun <- meanrep
        message("Set 'impfun' to mean replacement.")
      }
    }
    data <- object$data
    model <- object$model
    endrefl<- intersect(reflective(model), endogenous(model))
    qSquared <- matrix(NA, nrow=length(model$latent), ncol=1)
    colnames(qSquared) <- "Q-Squared"
    rownames(qSquared) <- model$latent
    sse <- function(model, data, dblind, i, impfun, total, ...){
        ## plsm <- sempls(model, impfun(dblind), pairwise=TRUE, verbose=FALSE, ...)
        plsm <- resempls(object, impfun(dblind), start = "ones",
                         method="ConstructLevelChanges", ...)
        #m <- length(model$blocks[[i]])
        #sse <- vector("numeric", length=m)
        m <- attr(plsm$data, "scaled:center")[model$blocks[[i]]]
        s <- attr(plsm$data, "scaled:scale")[model$blocks[[i]]]
        sse <- vector("numeric", length=length(m))
        for(l in 1:length(m)){
            # Estimation
            if(total){
                exogenous <- exogenous(model)
                yind <- which(i==model$latent)
                ind <- is.na(dblind[, model$blocks[[i]][l]])
                indf <- complete.cases(plsm$factor_scores[, exogenous])
                ind <- as.logical(ind*indf)
                e <- plsm$factor_scores[ind, exogenous, drop=FALSE] %*%
                     plsm$total_effects[exogenous, i, drop=FALSE] *
                     plsm$outer_loadings[model$blocks[[i]][l],i]
            }
            else{
                yind <- which(i==model$latent)
                ind <- is.na(dblind[, model$blocks[[i]][l]])
                indf <- complete.cases(plsm$factor_scores[, -yind])
                ind <- as.logical(ind*indf)
                e <- plsm$factor_scores[ind, -yind, drop=FALSE] %*%
                     plsm$path_coefficients[-yind, i, drop=FALSE] *
                     plsm$outer_loadings[model$blocks[[i]][l],i]
            }
            # Rescaling
            e <- e * s[l] + m[l]
            #print(table(is.na(e)))
            sse[l] <- sum((data[ind, model$blocks[[i]][l]] - e)^2, na.rm=TRUE)
        }
        return(sum(sse))
    }
    sso <- function(model, data, dblind, i){
        m <- try(apply(dblind[, model$blocks[[i]]], 2, mean, na.rm=TRUE), silent=TRUE)
        if(inherits(m, "try-error")) m <- mean(dblind[, model$blocks[[i]]], na.rm=TRUE)
        sso <- vector("numeric", length=length(m))
        for(k in 1:length(m)){
            ind <- is.na(dblind[, model$blocks[[i]][k]])
            sso[k] <- sum((data[ind, model$blocks[[i]][k]] - m[k])^2, na.rm=TRUE)
        }
        return(sum(sso))
    }
    if(dlines | is.null(d)) n <- nrow(data)
    if(is.null(d)){
        ## d <- n + 1   ## fixme
        d <- n - 1
        if(!dlines){
            dlines <- TRUE
            message("Set argument 'dlines' to: ", dlines, "\n")
        }
        message("Set argument 'd' to: ", d, "\n")
    }
    if(dlines) blindL <- split(sample(1:n), rep(1:d, length = n))
    for(i in endrefl){
        E <- vector("numeric", length=d)
        O <- vector("numeric", length=d)
        for(j in 1:d){
            dblind <- object$data
            if(dlines){
                ## fixme: [Mi 12. Feb 18:30:56 CET 2014]
                ## blind <- seq(j, n, by=d)
                blind <- blindL[[j]]
                dblind[blind,object$model$blocks[[i]]] <- NA
            }
            else{
                nobs <- prod(dim(object$data[,object$model$blocks[[i]]]))
                if(nobs==1) nobs <- nrow(dblind)
                blind <- seq(j, nobs, by=d)
                dblind[,object$model$blocks[[i]]][blind] <- NA
            }
            E[j] <- sse(model, data, dblind, i, impfun, total, ...)
            O[j] <- sso(model, data, dblind, i)
        }
        qSquared[i,] <- 1 - sum(E)/sum(O)
    }
    class(qSquared) <- "qSquared"
    return(qSquared)
}


# simple Mean replacement function
meanrep <- function(data){
    meanRepl <- function(x){
        mu <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- mu
        return(x)
    }
    return(apply(data, 2, meanRepl))
}

# function to evaluate different values for
# the omission distance 'd'
ommissionTest <- function(object, drange, ...){
    omt <- matrix(NA, nrow=length(object$model$latent),
                  ncol=length(drange))
    rownames(omt) <- object$model$latent
    colnames(omt) <- as.character(drange)
    for(i in drange){
        qSqrd <- try(semPLS:::qSquared(object, d=drange[i], ...),
                     silent=TRUE)
        #print(qSqrd)
        if(!inherits(qSqrd, "try-error")){
            #print(qSqrd)
            omt[,i] <- qSqrd
        }
    }
    return(omt)
}



print.qSquared <- function(x, na.print=".", digits=2, ...){
  print.table(x, na.print=na.print, digits=digits, ...)
  invisible(x)
}
