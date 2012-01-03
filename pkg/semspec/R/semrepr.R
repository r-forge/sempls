#' @include semspec.R
{}



### SEM representation object: #######################################

SEMREP_CLASS <- "semrepr"



parse_semspec <- function(object) {
  ## see lavaan/R/02lavaanUser.R: lavaanify and flatten.model.syntax

  structure(ret, class = SEMREP_CLASS)
}



### "Matrix" representation of the SEM specification: ################



#' @S3method model.frame semspec
model.frame.semspec <- function(formula, data) {
  ## TODO: data from formula$data or overwritten by data
  model.frame.semrep(parse_semspec(formula), data)
}



#' @S3method model.frame semrep
model.frame.semrepr <- function(formula, data) {
  ## see semPLS/R/plsm.R
  ## see lavaan/R/05lavaanModel.R: MATRIX representation of the model
}

