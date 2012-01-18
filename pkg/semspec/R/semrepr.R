#' @include semspec.R
{}



SEMREP_CLASS <- "semrepr"



#' @export
parse_semspec <- function(object) {
  ## see lavaan/R/02lavaanUser.R: lavaanify and flatten.model.syntax
  stopifnot(is_semspec(object))



  structure(ret, class = SEMREP_CLASS)
}



parse_rhs <- function(object) {

}



parse_lhs <- function(object) {

}




