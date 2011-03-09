# 01.03.2011
predict <- function(object, what=c("LVs", "MVs"), scale=c("original", "scaled"), total=FALSE){
    # total: only for MVs. Include exogenous and endogenous MVs
    #        Exogenous MVs are treated as if they were all reflective.
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
    # MVs
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
    # LVs
    if(what=="LVs" & length(fsMissing)==0){
        Y_hat <- object$factor_scores %*% object$path_coefficients
        return(Y_hat)
    }
    # MVs
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
    # LVs
    if(what=="LVs" & length(fsMissing)>0){
        Y_hat <- matrix(0, dim(object$factor_scores))
        for(i in endogen(model)){
            yind <- which(i==model$latent)
            Y_hat <- object$factor_scores[, -yind] %*%
                     object$path_coefficients[-yind, i]
        }
        return(Y_hat)
    }
    # MVs
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

