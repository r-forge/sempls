# core function for bootstrap of gof measures.
# if factor scores are required

bootX <- function(object, measure)
{
    nboot <- object$nboot
    data <- object$fitted_model$data
    # outer weights
    Wboot <- vector("list", length=nboot)
    # factor scores
    FSboot <- vector("list", length=nboot)
    # boot object has to supply the model
    model <- object$fitted_model$model
    bootIndices <- object$bootIndices
    for(i in 1:nboot){
        Wboot[[i]] <- matrix(ifelse(model$M, object$outer_weights[i,], 0), dim(model$M))
        dimnames(Wboot[[i]]) <- dimnames(model$M)
        FSboot[[i]] <- step4(data[bootIndices[i,],], outerW=Wboot[[i]], model, pairwise)
    }
    return(FSboot)
}


dgrho.bootsempls <- function(object)
{
    t0 <- dgrho(object$fitted_model)[,1]
    model <- object$fitted_model$model
    nboot <- object$nboot
    #bootIndices <- object$bootIndices
    #FSboot <- bootX(object)
    t <- matrix(NA, nrow=nboot, ncol=length(model$latent))
    colnames(t) <- model$latent
    for(i in 1:length(model$latent)){
        if(attr(model$blocks[[i]], "mode")=="B"){
            next
        }
        x <- object$t[, paste("lam", i, 1:length(model$blocks[[i]]), sep=""), drop=FALSE]
        if(ncol(x)==1){
            next
        }
        else {
            t[,i] <- rowSums(x)^2 / (rowSums(x)^2 + rowSums(1-x^2))
        }
    }
    ret <- object
    ret$t0 <- t0
    ret$t <- t
    ret$statistic <- dgrho
    class(ret) <- class(object)
    attr(ret, "statistic") <- "Dillon-Goldstein's rho"
    return(ret)
}




