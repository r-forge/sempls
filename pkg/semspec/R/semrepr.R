#' @include semspec.R
{}


SEMREP_CLASS <- "semrepr"


is_semrepr <- function(object) {
  class(object) %in% SEMREPR_CLASS
}



#' @export
semrepr <- function(object) {
  stopifnot(is_semspec(object))

  ret <- expand_semspec(object$model)
  ret <- flatten_semspec(ret)

  if ( !is.null(object$dataset) ) {
    group <- unique(ret$group)
    group <- group[!is.na(group)]
    levels <- lapply(object$dataset[group], levels)

    ret <- expand_semrepr_data(ret, levels)
  }

  if ( !is.null(object$constraints) ) {
    ret <- expand_semrepr_constraints(ret, object$constraints)
  }

  ## TODO: return the data and the constraints as well.

  structure(ret, class = c(SEMREP_CLASS, class(ret)))
}



#' @S3method plot semrepr
plot.semrepr <- function(x, y = NULL, ...) {
  stopifnot(require("qgraph"))

  n <- unique(c(x$lhs, x$rhs))
  n <- structure(seq(along = n), names = n)

  el <- cbind(n[x$lhs], n[x$rhs])
  dimnames(el) <- NULL

  qgraph(el)  # TODO make layout = "tree" work
}



### Expand semspec model: ############################################

expand_semspec <- function(object) {
  expand <- function(x) {
    attrs <- attributes(x)
    group <- find_group(x)
    if ( !is.null(group) ) {
      x <- remove_group(x)
    }
    x <- expand_formula(x)
    if ( !is.null(group) ) {
      f <- sprintf("%s | %s", deparse(x), deparse(group))
      x <- as.formula(f, env = environment(x))
    }
    attributes(x) <- attrs
    x
  }

  object$regression <- lapply(object$regression, expand)
  object$latent <- lapply(object$latent, expand)

  object
}



find_group <- function(term) {
  ## A bar is always on the right hand side.

  if ( (length(term[[3]]) >= 3) && term[[3]][[1]] == as.name("|") )
    term[[3]][[3]]
  else
    NULL
}



remove_group <- function(term) {
  ## We assume that there is a group on the right hand side, when
  ## calling this function.

  f <- paste(deparse(term[[2]]), deparse(term[[1]]),
             deparse(term[[3]][[2]]), collapse = "")
  as.formula(f, env = environment(term))
}



expand_formula <- function(term) {
  ## We want to expand both sides of a formula.

  if ( length(term) == 2 ) {
    rhs <- expand_term(term[[2]])
    f <- sprintf("~ %s", deparse(rhs))
  }

  if ( length(term) == 3 ) {
    rhs <- expand_term(term[[3]])
    lhs <- expand_term(term[[2]])
    f <- sprintf("%s ~ %s", deparse(lhs), deparse(rhs))
  }

  as.formula(f, env = environment(term))
}



expand_term <- function(term) {
  stopifnot(is.language(term))

  term <- formula(paste("~", deparse(term)))

  t <- terms(term)
  t <- attr(t, "term.labels")
  t <- paste(t, collapse = " + ")

  as.formula(sprintf("~ %s", t))[[2]]
}



### Flatten semspec model: ###########################################

flatten_semspec <- function(object) {
  group <- object$group
  object$group <- NULL

  ret <- list()
  for ( n in names(object) ) {
    ret[[n]] <- lapply(object[[n]], flatten_formula, n)
    ret[[n]] <- do.call(rbind, ret[[n]])
  }
  ret <- do.call(rbind, ret)
  rownames(ret) <- NULL

  if ( !is.null(group) ) {
    ret$group[is.na(ret$group)] <- as.character(group)
  }

  ret
}



flatten_formula <- function(term, what) {
  param <- attr(term, "param")

  group <- find_group(term)
  if ( !is.null(group) ) {
    term <- remove_group(term)
    group <- as.character(group)
  } else {
    group <- NA_character_
  }

  lhs <- as.character(flatten_term(term[[2]]))
  rhs <- as.character(flatten_term(term[[3]]))

  ret <- expand.grid(what = what, lhs = lhs,
                     rhs = rhs, lhsparam = NA_character_,
                     rhsparam = NA_character_, group = group,
                     stringsAsFactors = FALSE)
  attr(ret, "out.attrs") <- NULL


  lhsparam <- ret$lhs
  rhsparam <- ret$rhs
  if ( !is.null(param) ) {
    ml <- match(lhsparam, names(param), nomatch = 0)
    mr <- match(rhsparam, names(param), nomatch = 0)

    lhsparam[lhsparam %in% names(param)] <- param[ml]
    rhsparam[rhsparam %in% names(param)] <- param[mr]
  }
  ret$lhsparam <- lhsparam
  ret$rhsparam <- rhsparam


  ret
}



flatten_term <- function(term) {
  if ( length(term) > 1 )
    return(c(flatten_term(term[[2]]), term[[3]]))
  term
}



### Expand semrepr based on data and constraints: ####################

expand_semrepr_data <- function(object, groups) {
  ## NOTE: this is a hack, but does exactly what I want!

  ret <- object[1, ]
  ret$level <- NA_character_
  ret$param <- NA_character_

  n <- nrow(object)
  for ( i in seq(length = n) ) {
    x <- object[i, ]
    rownames(x) <- NULL

    level <- NA_character_
    if ( !is.na(x$group) ) {
      level <- groups[[x$group]]
    }

    d <- data.frame(x, level = level)
    d$param <- sprintf("%s_%s%s", d$lhsparam, d$rhsparam,
                       ifelse(is.na(d$level), "", sprintf("@%s", d$level)))

    ret <- rbind(ret, d)
  }

  ret <- ret[-1, ]
  rownames(ret) <- NULL

  ## ... and all parameters are at this point free:
  ret$free <- TRUE

  ret
}



expand_semrepr_constraints <- function(object, constraints) {
  ## We only allow constraints with one term on the left-hand side.

  unfree <- as.character(sapply(constraints, "[[", 2))
  object$free[object$param %in% unfree] <- FALSE
  object
}
