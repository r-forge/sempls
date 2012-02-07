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
  
  this_data <- object$dataset
  this_model <- as_semPLS_syntax(object, ...)
  sempls(model=this_model, data=this_data)
}

#' @export
as_semPLS_syntax <- function(object, ...) {
  stopifnot(is_semspec(object))

  repr <- semrepr(object)
  sm <- as.matrix(with(repr, repr[type=="structural",
                                  c("lhs", "rhs")]))
  mm <- as.matrix(with(repr, repr[type=="measurement",
                                  c("lhs", "rhs")]))

  plsm(data=object$dataset, strucmod=sm, measuremod=mm, ...)
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
  #ret <- paste(paths, ", ", parameters, ", ", value, "\n", sep="")
  #colnames(ret) <- NULL

  #tmp <- file()
  #cat(ret, file=tmp)
  #sem_model <- specifyModel(file = tmp)
  #close(tmp)
  
  ## Note: Variances for LVs are missing
  structure(ret, class = c("semmod"))
  #return(sem_model)
}



######################################################################


# TODO: Armin
start_values <- function(object, ...) {
  stopifnot(is_semspec(object))
  repr <- semrepr(object)
  # start values for repr$free repr$param
}


