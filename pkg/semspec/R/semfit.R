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
  
  model_parts <- as_semPLS_syntax(object, ...)
  
  model <- do.call(plsm, model_parts)
  
  fit <- sempls(model=model, data=model_parts$data, ...)
  object$fit <- fit
  
  repr <- semrepr(object)
  namesrepr <- names(repr)
  estimates <- coef(fit)
  Parameter <- strsplit(as.character(coef(fit)$Path), " -> ")
  estimates$Parameter <- sapply(Parameter, function(x) paste(x[2:1], collapse = "_"))
  erg <- merge(x = repr, y = estimates,
                by.x = "param", by.y = "Parameter", sort=FALSE)
  erg <- erg[, c(namesrepr, "Estimate")]
  names(erg) <- c(namesrepr, "semPLS_fit")
  return(erg)             
}

#' @export
as_semPLS_syntax <- function(object, ...) {
  stopifnot(is_semspec(object))

  repr <- semrepr(object)

  ### structural model
  sm <- as.matrix(with(repr, repr[type=="regression",
                                  c("rhs", "lhs")]))
  colnames(sm) <- c("from", "to")

  ### measurement model
  mm <- as.matrix(with(repr, repr[type=="latent",
                                  c("rhs", "lhs")]))
  colnames(mm) <- c("from", "to")
  return(list(data=object$dataset, strucmod=sm, measuremod=mm))
}



### sem: #############################################################

#' @export
semfit_sem <- function(object, start = start_values(object), ...) {
  stopifnot(require("sem"))
  stopifnot(is_semspec(object))

  this_model <- as_sem_syntax(object, ...)
  
  ### seems to be nessecary
  tmp <- file()
  cat(this_model, file = tmp)
  sem_model <- specifyEquations(file = tmp)
  close(tmp)

  ### startvalues
  sem_model <- as.data.frame(unclass(sem_model))
  sem_model <- merge(x=sem_model, y=start,
                     by.x="V2", by.y="param", all.x=TRUE)
  sem_model$start <- ifelse(!is.na(sem_model$val),
                            sem_model$val,
                            sem_model$V3)

  
  sem_model <- sem_model[, c("V1", "V2", "start")]
  sem_model <- as.matrix(sem_model)
  colnames(sem_model) <- NULL
  sem_model <- structure(sem_model, class="semmod")

  this_data <- object$dataset

  fit <- sem(model = sem_model, data = this_data)
  object$fit <- fit

  erg <- semrepr(object)
  erg$sem_fit <- coef(fit)[erg$param]
  return(erg)
}



#' @export
as_sem_syntax <- function(object, ...) {
  stopifnot(is_semspec(object))

  repr <- semrepr(object)

  ### which are fixed?
  summr <- summary(object)
  parameters <- repr$param
  fixed <- subset(summr$parameters$details,
                     subset = (Type == "Fixed"),
                     select = "Parameter")
  fixed <- unclass(fixed)$Parameter   # to get the character vector
  fixed_logical <- parameters %in% fixed   # logical


  ### only not fixed parameters
  ret <- with(repr[!fixed_logical & repr$type %in% c("latent", "regression"),],
           ifelse(type == "latent",
             paste(rhs, " = ", param, " * " , lhs, "\n", sep=""),
             paste(lhs, " = ", param, " * " , rhs, "\n", sep="")))
  
  covs <- with(repr[!fixed_logical & repr$type %in% c("covariance"),],
             paste("C(", rhs, ", ", lhs,") = ", param, "\n", sep=""))


  if(length(fixed) != 0){
  ### adding fixed parameters
  
  ## matrix with all constraints
  mconstr <- t(sapply(object$constraints, function(x) c(op=paste(x[1]),
                                               lhs=paste(x[2]),
                                               rhs=paste(x[3]))))
  
  ## values for fixed parameters
  values <- mconstr[mconstr[,"lhs"] %in% fixed, c("lhs", "rhs")]
  reprfixed <- merge(x=repr[fixed_logical,], y=values,
                     by.x="param", by.y="lhs", , suffixes = c(".name",".value"))

  
  ret2 <- with(reprfixed[reprfixed$type %in% c("latent", "regression"),],
            ifelse(type == "latent",
              paste(rhs.name, " = ", rhs.value," * " , lhs, "\n", sep=""),
              paste(lhs, " = ", rhs.value," * " , rhs, "\n", sep="")))

  covs2 <- with(reprfixed[reprfixed$type %in% c("covariance"),],
             ifelse(type == "covariance",
               paste("C(", rhs.name, ", ", lhs,") = ", rhs.value, "\n", sep="")))

               
  ret <- c(ret, ret2)
  covs <- c(covs, covs2)
  }


  return(c(ret, covs))
}




######################################################################


## TODO: Armin
start_values <- function(object, ...) {
  stopifnot(is_semspec(object))
  repr <- semrepr(object)
  ### which are fixed?
  summr <- summary(object)
  parameters <- repr$param
  fixed <- subset(summr$parameters$details,
                     subset = (Type == "Fixed"),
                     select = "Parameter")
  fixed <- unclass(fixed)$Parameter   # to get the character vector
  fixed_logical <- parameters %in% fixed   # logical
  ### start values for repr$free repr$param
  start <- repr[!fixed_logical, "param", drop=FALSE]
  start$val <- NA
  val <- c(...)
  start[match(names(val), start$param), "val"] <- val
  return(start)
}


