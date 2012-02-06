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



as_lavaan_syntax <- function(object) {
  stopifnot(is_semspec(object))

  repr <- semrepr(object)



}




### semPLS: ##########################################################

semfit_semPLS <- function(object, ...) {
  stopifnot(require("semPLS"))
  stopifnot(is_semspec(object))
}


as_semPLS_syntax <- function(object) {
  stopifnot(is_semspec(object))

  repr <- semrepr(object)

}



### sem: #############################################################

#' @export
semfit_sem <- function(object, start = start_values(object), ...) {
  stopifnot(require("sem"))
  stopifnot(is_semspec(object))
}



#' @export
as_sem_syntax <- function(object, ...) {
  stopifnot(is_semspec(object))

  repr <- semrepr(object)

  arrows <- ifelse(repr$type == "covariance", "<->", "->")
  paths <- paste(repr$lhs, arrows, repr$rhs)

  parameters <- repr$param
  parameters[!repr$free] <- NA  # NOTE: only fixed

  value <- rep(NA, length(parameters))

  ret <- cbind(paths, parameters, value)
  colnames(ret) <- NULL

  structure(ret, class = c("semmod"))
}



######################################################################

# TODO: Armin
start_values <- function(object, ...) {
  stopifnot(is_semspec(object))
  repr <- semrepr(object)
  # start values for repr$free repr$param
}
