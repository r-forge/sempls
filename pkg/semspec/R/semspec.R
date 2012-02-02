


SEMSPEC_CLASS <- "semspec"



is_semspec <- function(object) {
  any(class(object) %in% SEMSPEC_CLASS)
}



#' @export
`+.semspec` <- function(e1, e2) {
  stopifnot(is_semspec(e1))
  stopifnot(is_semspec(e2))

  l <- list(model = NULL, dataset = e1$dataset, constraints = NULL)
  l$model <- concatenate_model(e1$model, e2$model)
  l$constraints <- c(e1$constraints, e2$constraints)

  if ( !is.null(e2$dataset) ) {
    l$dataset <- e2$dataset
  }

  structure(l, class = SEMSPEC_CLASS)
}



concatenate_model <- function(e1, e2) {
  names <- unique(c(names(e1), names(e2)))

  l <- lapply(names,
              function(n) {
                if ( n %in% c("group") ) {
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
  l
}



### Model definition language: #######################################

semspec_base <- function(what, formula, call, param = NULL) {
  l <- list(model = list(list(formula = formula)))
  names(l[[1]]) <- what

  attr(l[[1]][[1]][[1]], "call") <- call
  attr(l[[1]][[1]][[1]], "param") <- param

  structure(l, class = SEMSPEC_CLASS)
}



#' @export
measurement <- function(formula, param = NULL) {
  semspec_base("measurement", substitute(formula), match.call(), param)
}
# lavaan: regression



#' @export
structural <- function(formula, param = NULL) {
  semspec_base("structural", substitute(formula), match.call(), param)
}
# lavaan: latent


#' @export
covariance <- function(formula) {
  semspec_base("covariance", substitute(formula), match.call())
}



#' @export
intercept <- function(formula) {
  semspec_base("intercept", substitute(formula), match.call())
}



#' @export
group <- function(formula) {
    semspec_base("group", substitute(formula), match.call())
}



### Data definition language: ########################################

#' @export
dataset <- function(data) {
  l <- list(data)
  names(l) <- "dataset"
  attr(l[[1]], "call") <- match.call()

  structure(l, class = c("semspec_dataset", SEMSPEC_CLASS))
}



#' @S3method print semspec_dataset
print.semspec_dataset <- function(x, ...) {
  print(attr(x[[1]], "call"))
}



### Constraints definition language: #################################

#' @export
constraint <- function(expression) {
  l <- list(list(substitute(expression)))
  names(l) <- "constraints"
  attr(l[[1]][[1]], "call") <- match.call()

  structure(l, class = c("semspec_constraint", SEMSPEC_CLASS))
}



#' @S3method print semspec_data
print.semspec_constraint <- function(x, ...) {
  print(attr(x[[1]], "call"))
}

