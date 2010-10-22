plot.sempls <- function(x, ...){
    col <- list(...)$col
    #if(is.null(col) && require(colorspace)){
    #    for(i in 0:(length(x$model$latent)-1)){
    #        n <- sapply(x$model$blocks, length)
    #        col_tmp <- rainbow_hcl(n[i+1], start = 90+(i*777), end = -30+(i*777))
    #        col <- append(col, col_tmp)
    #    }
    #}
    if(!is.null(col)){
        old_col <- trellis.par.get("superpose.line")$col
        trellis.par.set(superpose.line=list(col=col))
    }
    MVs <- NULL
    #ymin <- min(x$weights_evolution$iteration)
    #ymax <- max(x$weights_evolution$iteration)
    print(xyplot(weights ~ iteration | LVs,
                   groups=MVs, type="b",
                   data=x$weights_evolution,
                   as.table=TRUE,
                   auto.key=list(lines=TRUE,
                                 space="right",
                                 title="MVs",
                                 ...),
                   main="Evolution of outer weights",
                   xlab="Iteration",
                   ylab="Outer Weights",
                   #ylim=ymin:ymax,
                   ...))
    if(!is.null(col)){
        trellis.par.set(superpose.polygon=list(col=old_col))
    }
    invisible(x$weights_evolution)
}

### Alternatives:
#xyplot(weights ~ iteration|LVs, data=tmp2, groups=MVs, type = "a", auto.key =list(space = "right", points = FALSE, lines = TRUE), ylim=range(weights))
#tmp <- tmp[tmp$weights!=0,]
#tmp4$LVs <- factor(tmp4$LVs, levels=ecsi$model$latent)
#xyplot(weights ~ iteration|LVs, data=tmp4, groups=MVs, as.table=TRUE, type="b", auto.key =list(space = "rig#ht", points = FALSE, lines = TRUE))
#xyplot(weights ~ iteration, data=tmp4, groups=MVs, as.table=TRUE, type="l", col=1)


### lattice:::densityplot
densityplot.sempls <- function(x, data, use=c("fscores", "prediction", "residuals"), ...){
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
    Y$name <- factor(Y$name, levels=x$model$latent)
    if(is.null(list(...)$main)){
        main=paste(deparse(substitute(x)), "\n", ifelse(use=="fscores", "factor scores", use))
    }
    if(is.null(list(...)$sub)){
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

mvplot <- function(model, data, ...){
  UseMethod("mvplot", model)
}

mvplot.plsm <- function(model, data, ask=TRUE, ...){
    try(data <- data[, model$manifest], silent=TRUE)
    if(inherits(data, "try-error")) stop("The 'models' manifest variables must be contained in 'data'!")

    long <- reshape(data, v.names="value",  ids=rownames(data), idvar="ids",
                    times=names(data), timevar="MV", varying=list(names(data) ),
                    direction="long")
    opar <- par(no.readonly=TRUE)
    on.exit(par(opar))
    par(ask=TRUE)
    charts <- list()
    for(i in model$latent){
        tab <- as.data.frame(xtabs(~ value + MV ,
                                   data=long[long$MV %in% model$block[[i]],]))
        charts[[i]] <- barchart(Freq ~ value| MV, data=tab, main=i, ...)
        print(charts[[i]])
    }
    invisible(charts)
}

mvpairs <- function(model, data, ...){
  UseMethod("mvpairs", model)
}

mvpairs.plsm <- function(model, data, ask=TRUE, ...){
    opar <- par(no.readonly=TRUE)
    on.exit(par(opar))
    par(ask=TRUE)
    for(i in model$latent){
        if(length(model$blocks[[i]])==1) next
        pairs(data[,model$blocks[[i]]],
              lower.panel=panel.jitter,
              diag.panel=panel.bars,
              upper.panel=panel.cor,
              cex.labels=2, font.labels=1, main=i,
              ...)
    }

}

panel.bars <- function(x, offset=0.02, ...){
    dots <- list(...)
    barcol <- dots$barcol
    dots$col <- NULL
    if(is.null(barcol)) barcol <- "lightgrey"
    col <- barcol
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    tab <- table(x)
    b <- barplot(tab, plot=FALSE)
    breaks <- as.numeric(names(tab))
    nB <- length(breaks)
    y <- tab/max(tab)
    rect(xleft=(breaks - 0.43), ybottom=offset, xright=(breaks + 0.43),
         ytop=(y + offset), col=barcol, ...)
}

#col = par("col")
panel.jitter <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 3/3, iter = 3, ...){
    if(is.ordered(x)) x <- as.numeric(x)
    if(is.ordered(y)) x <- as.numeric(y)
    points(jitter(x), jitter(y), pch = pch, bg = bg, cex = cex, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
}

panel.cor <- function(x, y, digits=2, postfix="", cex.cor, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    xyData <- data.frame(x=x, y=y)
    r <- abs(cor(x, y, use="pairwise.complete.obs", ...))
    txt1 <- format(r, digits=digits)
    txt1 <- paste("r = ", txt1, "\n\n", sep="")
    compl <- sum(complete.cases(xyData))
    n <- nrow(xyData)
    perc <- 100*compl/n
    perc <- format(perc, digits=1)
    txt2 <- paste("N = ", compl, " (", perc, "%)", sep="")
    #if(nchar(postfix)==0) postfix <- "pairwise complete"
    txt <- paste(txt1, txt2, postfix, sep="")
    #if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    if(missing(cex.cor)) cex.cor <- strwidth(txt)
    #text(0.5, 0.5, txt2, cex = cex.cor * r)
    text(0.5, 0.5, txt)
}




