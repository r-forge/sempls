#' @include semspec.R
#' @include semrepr.R
{}



### Lavaan: ##########################################################

semfit_lavaan <- function(object, ...) {
  stopifnot(require("lavaan"))
  stopifnot(is_semspec(object))

  data <- object$data
  constr <- object$constraints

  repr <- semrepr(object)
  model <- as_lavaan(object)
}



as_lavaan <- function(object, ...) {
  stopifnot(is_semrepr(object))

}




### semPLS: ##########################################################

semfit_semPLS <- function(object, ...) {
  stopifnot(require("semPLS"))
  stopifnot(is_semspec(object))
}





### sem: #############################################################

semfit_sem <- function(object, ...) {
  stopifnot(require("sem"))
  stopifnot(is_semspec(object))
}



