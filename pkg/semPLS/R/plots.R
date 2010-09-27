plot.sempls <- function(x, ...){
    col <- list(...)$col
    if(is.null(col) && require(colorspace)){
        for(i in 0:(length(x$model$latent)-1)){
            n <- sapply(x$model$blocks, length)
            col_tmp <- rainbow_hcl(n[i+1], start = 90+(i*777), end = -30+(i*777))
            col <- append(col, col_tmp)
        }
    }
    if(!is.null(col)){
        old_col <- trellis.par.get("superpose.polygon")$col
        trellis.par.set(superpose.polygon=list(col=col))
    }
    MVs <- NULL
    ymin <- min(x$weights_evolution$iteration)
    ymax <- max(x$weights_evolution$iteration)
    print(barchart(iteration ~ weights | LVs,
                   groups=MVs, stack=TRUE, horizontal=TRUE,
                   data=x$weights_evolution,
                   as.table=TRUE,
                   auto.key=list(rectangles=TRUE,
                                 space="right",
                                 title="MVs",
                                 ...),
                   main="Evolution of outer weights",
                   xlab="Outer Weights",
                   ylab="Iteration",
                   ylim=ymin:ymax,
                   ...))
    if(!is.null(col)){
        trellis.par.set(superpose.polygon=list(col=old_col))
    }
    invisible(x$weights_evolution)
}

### Alternatives:
#xyplot(weights ~ iteration|LVs, data=tmp2, groups=MVs, type = "a", auto.key =list(space = "right", points = FALSE, lines = TRUE), ylim=c(0.00001,1))
#tmp <- tmp[tmp$weights!=0,]
#tmp4$LVs <- factor(tmp4$LVs, levels=ecsi$model$latent)
#xyplot(weights ~ iteration|LVs, data=tmp4, groups=MVs, as.table=TRUE, type="b", auto.key =list(space = "right", points = FALSE, lines = TRUE))
#xyplot(weights ~ iteration, data=tmp4, groups=MVs, as.table=TRUE, type="l", col=1)


### lattice:::densityplot
densityplot.sempls <- function(x, data, use=c("fscores", "prediction", "residuals"),
                               main, sub, ...){
    use <- match.arg(use)
    if(use=="fscores")         val <- x$factor_scores
    else if(use=="prediction") val <- predict(x)
    else if(use=="residuals")  val <- residuals(x)

    Y <- data.frame(NULL)
    exogenous <- exogen(x$model)
    for(i in x$model$latent){
        if(i %in% exogenous & use!="fscores") next
        tmp <- data.frame(value=val[,i], name=i)
        Y <- rbind(Y, tmp)
    }
    if(missing(main)) main=paste(deparse(substitute(x)), "\n", ifelse(use=="fscores", "factor scores", use))
    if(missing(sub)){
        sub=paste("Exogenous LVs: ", paste(exogenous, collapse=", "))
    }
    densityplot(~value|name, data=Y, main=main, sub=sub, as.table=TRUE, ...)
 }

densityplot.bootsempls <- function(x, data, pattern="beta", subset=NULL, ...){
    ind <- grep(pattern, colnames(x$t))
    ifelse(is.null(subset),
           params <- colnames(x$t)[ind],
           ifelse(is.character(subset), params <- subset, params <- colnames(x$t)[subset])
           )
    Y <- data.frame(NULL)
    for(i in params){
        if(round(var(x$t[,i], na.rm=TRUE), digits=4)==0) next
        tmp <- data.frame(value=x$t[,i], name=i)
        Y <- rbind(Y, tmp)
    }
    densityplot(~value|name, data=Y, as.table=TRUE, ...)
 }

# lattice:::parallel
parallel.bootsempls <- function(x, data, pattern="beta", subset=NULL, reflinesAt,
                                col=c("grey", "darkred", "darkred", "black"),
                                lty=c("solid", "solid", "dashed", "dotted"), ...){
    ifelse(is.null(subset), ind <- grep(pattern, colnames(x$t)), ind <- subset)
    lower <- summary(x, ...)$table$Lower
    upper <- summary(x, ...)$table$Upper
    Y <- rbind(x$t, x$t0, lower, upper, deparse.level=0)
    if(!missing(reflinesAt)){
        Y <- rbind(Y, matrix(rep(reflinesAt, each=ncol(x$t)), nrow=length(reflinesAt), byrow=TRUE))
        origin <- c(rep("1resample", x$nboot), "2sample", "3ci", "3ci",
                    rep("4reflines", times=length(reflinesAt)))
        Y <- data.frame(Y, origin)
    }
    else Y <- data.frame(Y, origin=c(rep("1resample", x$nboot), "2sample", "3ci", "3ci"))
    parallel(~Y[ind], data=Y, groups=origin, common.scale=TRUE, col=col, lty=lty, ...)
 }
