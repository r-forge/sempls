

# Dillon-Goldstein's rho (Composite Reliability in SmartPLS)
dgrho <- function(object){
    dgr <- matrix(NA, nrow=length(object$model$latent), ncol=2)
    rownames(dgr) <- object$model$latent
    colnames(dgr) <- c("Dillon-Goldstein's rho", "reflective MVs")
    for(i in object$model$latent){
        if(attr(object$model$blocks[[i]], "mode")=="B"){
            next
        }
        x <- object$outer_loadings[, i]
        ind <- which(x!=0)
        if(length(ind)==1){
            dgr[i,2] <- 1
            next
        }
        else {
            x <- x[ind]
            dgr[i,1] <- sum(x)^2 / (sum(x)^2 + sum(1-x^2))
            dgr[i,2] <- length(ind)
        }
    }
    return(dgr)
}

comunality <- function(object){
    com <- matrix(NA, nrow=length(object$model$latent), ncol=2)
    rownames(com) <- object$model$latent
    colnames(com) <- c("comunality", "reflective MVs")
    for(i in object$model$latent){
        if(attr(object$model$blocks[[i]], "mode")=="B"){
            next
        }
        x <- object$outer_loadings[, i]
        ind <- which(x!=0)
        if(length(ind)==1){
            com[i,2] <- 1
            next
        }
        else {
            x <- x[ind]
            com[i,1] <- 1/length(x)*sum(x^2)
            com[i,2] <- length(ind)
        }
    }
    return(com)
}

print.comunality <- function(object){
    print(object, digits=3)
    aveCom <- sum(object[,2], na.rm=TRUE)^-1 * sum(object[,1] * object[,2], na.rm=TRUE)
    paste("Average comunality:", round(aveCom, digits=3))
}



# Redundancy Example:
redundancy <- function(object){
    red <- as.matrix(comunality(object)[,1] * rSquared(object)[,1])
    colnames(red) <- "redundancy"
    return(red)
}

print.redundancy <- function(object){
    print(object, digits=3)
    aveRed <- nrow(object)^-1 * sum(object[,1], na.rm=TRUE)
    paste("Average redundancy:", round(aveRed, digits=3))
}

predict <- function(object, what=c("LVs", "MVs"), scale=c("original", "scaled"), total=FALSE){
    what <- match.arg(what)
    scale <- match.arg(scale)
    model <- object$model
    data <- object$data
    # missing factor scores?
    fsMissing <- which(complete.cases(object$factor_scores)==FALSE)
    # mising MV data?
    mvMissing <- which(complete.cases(object$data)==FALSE)
    if(what=="MVs"){
        msep <- function(object, mvPrediction, mvObserved){
            sum((mvPrediction - mvObserved)^2, na.rm=TRUE)/
            (prod(dim(data)) - ncol(object$coeff) - sum(is.na(mvPrediction)))
        }
    }

    if(total & what=="MVs"){
        mv_hat <- matrix(NA, nrow=nrow(data), ncol=ncol(data))
        colnames(mv_hat) <- colnames(data)
        exogen <- exogen(model)
        for(i in endogen(model)){
            block <- model$blocks[[i]]
            m <- attr(data, "scaled:center")[block]
            s <- attr(data, "scaled:scale")[block]
            for(l in 1:length(m)){
                ind <- complete.cases(object$factor_scores[, exogen])
                e <- object$factor_scores[ind, exogen, drop=FALSE] %*%
                     object$total_effects[exogen, i, drop=FALSE] *
                     object$outer_loadings[block[l],i]
                # rescaling
                if(scale=="original") e <- e * s[l] + m[l]
                mv_hat[ind, block[l]] <- e
            }
        }
        for(i in exogen(model)){
            block <- model$blocks[[i]]
            m <- attr(data, "scaled:center")[block]
            s <- attr(data, "scaled:scale")[block]
            for(l in 1:length(m)){
                ind <- complete.cases(object$factor_scores[, i])
                e <- object$factor_scores[ind, i] *
                     object$outer_loadings[block[l],i]
                # rescaling
                if(scale=="original") e <- e * s[l] + m[l]
                mv_hat[ind, block[l]] <- e
            }
        }
        result <- list(mv_prediction=mv_hat,
                       msep=msep(object, mv_hat, ifelse(scale=="orig", rescale(object), data)))
        return(result)
    }



    # situation A: complete data
    if(what=="LVs" & length(fsMissing)==0){
        Y_hat <- object$factor_scores %*% object$path_coefficients
        return(Y_hat)
    }
    if(what=="MVs" & length(fsMissing)==0){
        if(length(mvMissing)==0){
            mv_hat <- object$factor_scores %*%
                      object$path_coefficients %*%
                      t(object$outer_loadings)
            for(i in exogen(model)){
                block <- model$blocks[[i]]
                m <- attr(data, "scaled:center")[block]
                s <- attr(data, "scaled:scale")[block]
                for(l in 1:length(m)){
                    e <- object$factor_scores[, i] *
                         object$outer_loadings[block[l],i]
                    mv_hat[, block[l]] <- e
                }
            }
            result <- list(mv_prediction=mv_hat,
                           msep=msep(object, mv_hat, data))
        }
        if(length(mvMissing)==0 & scale=="original"){
            rescale <- function(object, data){
                m <- attr(object$data, "scaled:center")
                s <- attr(object$data, "scaled:scale")
                if(missing(data)){
                    t(apply(object$data, 1, function(x) {x * s + m}))
                }
                else t(apply(data, 1, function(x) {x * s + m}))
            }
            mv_hat <- rescale(object, mv_hat)
            result <- list(mv_prediction=mv_hat,
                           msep=msep(object, mv_hat, rescale(object)))
        }
        return(result)
    }

    # situation B: with missing observations
    if(what=="LVs" & length(fsMissing)>0){
        Y_hat <- matrix(0, dim(object$factor_scores))
        for(i in endogen(model)){
            yind <- which(i==model$latent)
            Y_hat <- object$factor_scores[, -yind] %*%
                     object$path_coefficients[-yind, i]
        }
        return(Y_hat)
    }

    if(what=="MVs" & length(fsMissing)>0){
        mv_hat <- matrix(NA, nrow=nrow(data), ncol=ncol(data))
        colnames(mv_hat) <- colnames(data)
        pred <- predecessors(model)
        for(i in endogen(model)){
            block <- model$blocks[[i]]
            m <- attr(data, "scaled:center")[block]
            s <- attr(data, "scaled:scale")[block]
            for(l in 1:length(m)){
                ind <- complete.cases(object$factor_scores[, pred[[i]]])
                e <- object$factor_scores[ind, pred[[i]], drop=FALSE] %*%
                     object$path_coefficients[pred[[i]], i, drop=FALSE] *
                     object$outer_loadings[block[l],i]
                # rescaling
                if(scale=="original") e <- e * s[l] + m[l]
                mv_hat[ind, block[l]] <- e
            }
        }
        for(i in exogen(model)){
            block <- model$blocks[[i]]
            m <- attr(data, "scaled:center")[block]
            s <- attr(data, "scaled:scale")[block]
            for(l in 1:length(m)){
                ind <- complete.cases(object$factor_scores[, i])
                e <- object$factor_scores[ind, i] *
                     object$outer_loadings[block[l],i]
                # rescaling
                if(scale=="original") e <- e * s[l] + m[l]
                mv_hat[ind, block[l]] <- e
            }
        }
        result <- list(mv_prediction=mv_hat,
                       msep=msep(object, mv_hat, ifelse(scale=="orig", rescale(object), data)))
        return(result)
    }
}



residuals <- function(object, what=c("LVs", "MVs"), scale=c("original", "scaled"), total=FALSE){
    what <- match.arg(what)
    scale <- match.arg(scale)
    model <- object$model
    data <- object$data
    
    if(what=="LVs"){
        res <- object$factor_scores - predict(object, what, scale, total)
    }
    else{
      if(scale=="scaled"){
        pdata <- predict(object, what, scale, total)$mv_prediction
        res <- data - pdata
      }
      else{
        m <- attr(data, "scaled:center")
        s <- attr(data, "scaled:scale")
        data <- t(t(data) * s) + m
        pdata <- predict(object, what, scale, total)$mv_prediction
        res <- data - pdata
      }
    }
    return(res)
}


rSquared <- function(object, na.rm=FALSE, ...){
  Y_hat <- predict(object, ...)
  if(sum(is.na(Y_hat)) > 0 & !na.rm) stop("Use argument 'na.rm=TRUE'!")
  R_squared <- apply(Y_hat, 2, var, na.rm=na.rm) / apply(object$factor_scores, 2, var, na.rm=na.rm)
  R_squared[R_squared==0] <- 0
  R_squared <- as.matrix(R_squared)
  R_squared <- cbind(R_squared, colSums(object$model$D))
  colnames(R_squared) <- c("R-squared", "predecessors")
  return(R_squared)
}

print.rSquared <- function(object){
    print(object, digits=3)
    aveRsquared <- nrow(object)^-1 * sum(object[,1], na.rm=TRUE)
    paste("Average R-squared:", round(aveRsquared, digits=3))
}

gof <- function(object){
    rSq <- rSquared(object)
    aveRsq <- nrow(rSq)^-1 * sum(rSq[,1], na.rm=TRUE)
    com <- comunality(object)
    aveCom <- sum(com[,2], na.rm=TRUE)^-1 * sum(com[,1] * com[,2], na.rm=TRUE)
    gof <- sqrt(aveCom * aveRsq)
    return(gof)
}
