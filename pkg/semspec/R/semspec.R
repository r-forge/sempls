


### SEM specification object: ########################################

SEMSPEC_CLASS <- "semspec"



is_semspec <- function(object) {
  class(object) %in% SEMSPEC_CLASS
}



semspec_formula <- function(formula, group, what) {
  l <- list(list(list(formula = formula, group = group)))
  names(l) <- what

  structure(l, class = SEMSPEC_CLASS)
}



semspec_data <- function(data) {
  l <- list(list(data))
  names(l) <- "data"

  structure(l, class = SEMSPEC_CLASS)
}



semspec_variable <- function(variable, what) {
  l <- list(list(variable))
  names(l) <- what

  structure(l, class = SEMSPEC_CLASS)
}



semspec_constraint <- function(constraint) {
  l <- list(list(constraint))
  names(l) <- "constraint"

  structure(l, class = SEMSPEC_CLASS)
}



#' @export
`+.semspec` <- function(e1, e2) {
  stopifnot(is_semspec(e1))
  stopifnot(is_semspec(e2))

  names <- unique(c(names(e1), names(e2)))

  l <- lapply(names,
              function(n) {
                if ( n %in% c("group", "data") ) {
                  if ( length(e2[[n]]) > 0 ) {
                    e2[[n]]
                  }
                  else {
                    e1[[n]]
                  }
                } else {
                  c(e1[[n]], e2[[n]])
                }
              })
  names(l) <- names

  structure(l, class = SEMSPEC_CLASS)
}



has_formula <- function(object) {
  stopifnot(is_semspec(object))
  length(object$latent) > 0 |
  length(object$regression) > 0 |
  length(object$covariance) > 0
}



has_data <- function(object) {
  stopifnot(is_semspec(object))
  length(object$data) > 0
}



has_global_group <- function(object) {
  stopifnot(is_semspec(object))
  length(object$group) > 0
}



has_constraint <- function(object) {
  stopifnot(is_semspec(object))
  length(object$constraint) > 0
}


has_intercept <- function(object) {
  stopifnot(is_semspec(object))
  length(object$intercept) > 0
}



### SEM specification syntax: ########################################



#' @export
latent <- function(formula, group = NULL) {
  semspec_formula(substitute(formula), substitute(group), "latent")
}



#' @export
regression <- function(formula, group = NULL) {
  semspec_formula(substitute(formula), substitute(group), "regression")
}



#' @export
covariance <- function(formula, group = NULL) {
  ## TODO: check if variance?
  semspec_formula(substitute(formula), substitute(group), "covariance")
}



#' @export
data <- function(data) {
  stopifnot(is.data.frame(data))
  semspec_data(data)
}



#' @export
group <- function(variable) {
  semspec_variable(substitute(variable), "group")
}



#' @export
intercept <- function(variable) {
  semspec_variable(substitute(variable), "intercept")
}



#' @export
constraint <- function(constraint) {
  semspec_constraint(substitute(constraint))
}



### SEM specification convenience functions: #########################



#' @S3method print semspec
#print.semspec <- function(x, ...) {
#  cat("SEM specification object\n")
#}


