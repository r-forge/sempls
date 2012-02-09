#' @include semspec.R
#' @include semrepr.R
{}



#' @S3method print semspec
print.semspec <- function(x, ...) {
  cat("Structural equation model specification\n")
  print(semrepr(x))
  cat("\n")
  cat(ifelse(is.null(x$dataset), "No", "A"), "dataset and",
      length(x$constraints), "constraint(s) specified\n")
}



#' @S3method summary semspec
summary.semspec <- function(object, ...) {
  repr <- semrepr(object)

  ret <- list()
  ret$formula <- deparse_semspec(object)
  ret$variables <- summarize_variables(object, repr)
  ret$parameters <- summarize_parameters(object, repr)
  ret$constraints <- summarize_constraints(object, repr)
  ret$data <- summarize_data(object, repr)
  ret$df <- model_df(ret$variables$count, ret$parameters$count)


  structure(ret, class = c("summary.semspec", class(ret)))
}



#' @S3method print summary.semspec
print.summary.semspec <- function(x, ...) {
  cat("Structural equation model specification\n")

  cat("\n", x$formula, "\n\n", sep = "")
  x$formula <- NULL

  n <- length(x)
  for ( i in seq(length = n) ) {
    print(x[[i]], ...)
    if ( i < n )
      cat("\n")
  }

  invisible(x)
}



deparse_semspec <- function(spec) {
  stopifnot(is_semspec(spec))

  calls <- c(unlist(lapply(spec$model,
                           function(x)
                           lapply(x, attr, "call"))),
             attr(spec$dataset, "call"),
             lapply(spec$constraints, attr, "call"))

  calls <- lapply(calls, deparse)
  calls <- lapply(calls,
                  function(x)
                  tidy.source(text = x, output = FALSE)$text.tidy)
  calls <- paste("  ", calls, sep = "", collapse = "\n")
  calls
}



### Degrees of freedom: ##############################################

#' @export
summarize_df <- function(spec, repr = NULL) {
  stopifnot(is_semspec(spec))
  stopifnot(is.null(repr) | is_semrepr(repr))

  if ( is.null(repr) )
    repr <- semrepr(spec)

  vars <- summarize_variables(spec, repr)
  params <- summarize_parameters(spec, repr)

  model_df(vars$count, params$count)
}



model_df <- function(variables, parameters) {
  manifest <- variables["Manifest"]
  free <- parameters["Free"] + parameters["Restricted"]

  t <- free
  p <- (manifest/2) * (manifest + 1)

  count <- c(DF = unname(p - t))
  details <- data.frame(estimates = unname(t),
                        empiricals = unname(p))

  summarized_result("df", count, details)
}



#' @S3method print summarized_df
print.summarized_df <- function(x, ...) {
  cat("Degrees of freedom:", x$count, "\n")
  invisible(x)
}



### Summarize variables: #############################################

#' @export
summarize_variables <- function(spec, repr = NULL) {
  stopifnot(is_semspec(spec))
  stopifnot(is.null(repr) | is_semrepr(repr))

  model_vars <- unique(model_variables(spec$model))
  data_vars <- colnames(spec$dataset)

  manifest <- model_vars %in% data_vars

  type <- rep("Latent", length(model_vars))
  type[manifest] <- "Manifest"
  type <- factor(type, levels = c("Latent", "Manifest"))

  count <- c(Number = length(model_vars), table(type))
  details <- data.frame_str(Variable = model_vars, Type = type)

  summarized_result("variables", count, details)
}



model_variables <- function(model) {
  vars <- function(x) {
    group <- find_group(x)
    if ( !is.null(group) ) {
      x <- remove_group(x)
    }
    unique(all.vars(x))
  }

  model$group <- NULL
  unique(unname(unlist(sapply(model, function(x) lapply(x, vars)))))
}



#' @S3method print summarized_variables
print.summarized_variables <- function(x, ...) {
  vars <- split(x$details$Variable, x$details$Type)

  out <- x$count
  names(out)[1] <- "Variables:"
  print(out)

  if ( x$count[1] > 0 ) {
    for ( var in names(vars) ) {
      out <- paste(vars[[var]], collapse = ", ")
      out <- paste(strwrap(out, indent = 6, exdent = 6), collapse = "\n")
      cat("  ", var, ":\n", out, "\n", sep = "")
    }
  }

  invisible(x)
}



### Summarize parameters: ############################################

#' @export
summarize_parameters <- function(spec, repr = NULL) {
  stopifnot(is_semspec(spec))
  stopifnot(is.null(repr) | is_semrepr(repr))

  if ( is.null(repr) )
    repr <- semrepr(spec)

  constraints <- spec$constraints

  type <- rep("Free", length(repr$param))
  names(type) <- repr$param

  if ( !is.null(constraints) ) {
    type[fixed_parameters(constraints, repr$param)] <- "Fixed"
    type[restricted_parameters(constraints, repr$param)] <- "Restricted"
  }

  type <- factor(type, levels = c("Free", "Fixed", "Restricted"))

  count <- c(Number = length(type), table(type))
  details <- data.frame_str(Parameter = value(repr$param, "character"),
                            Type = type)

  summarized_result("parameters", count, details)
}



fixed_parameters <- function(constraints, parameters) {
  unfree <- constrained_parameters(constraints)
  fixed <- sapply(constraints, is_fixed_constraint)
  active <- sapply(constraints, is_active_constraint, parameters)

  unfree[fixed & active]
}



restricted_parameters <- function(constraints, parameters) {
  unfree <- constrained_parameters(constraints)
  fixed <- sapply(constraints, is_fixed_constraint)
  active <- sapply(constraints, is_active_constraint, parameters)

  unfree[!fixed & active]
}



is_fixed_constraint <- function(constraint) {
  ## TODO: allow functions which return a value?
  ## TODO: vectorize
  constraint[[1]] == "==" & is.numeric(constraint[[3]])
}



#' @S3method print summarized_parameters
print.summarized_parameters <- function(x, ...) {
  params <- split(x$details$Parameter, x$details$Type)

  out <- x$count
  names(out)[1] <- "Parameters:"
  print(out)

  if ( x$count[1] > 0 ) {
    for ( param in names(params) ) {
      out <- paste(params[[param]], collapse = ", ")
      out <- paste(strwrap(out, indent = 6, exdent = 6), collapse = "\n")
      cat("  ", param, ":\n", out, "\n", sep = "")
    }
  }

  invisible(x)
}



### Summarize constraints: ###########################################

#' @export
summarize_constraints <- function(spec, repr = NULL) {
  stopifnot(is_semspec(spec))
  stopifnot(is.null(repr) | is_semrepr(repr))

  if ( is.null(repr) )
    repr <- semrepr(spec)

  constraints <- spec$constraints
  data <- spec$dataset

  active <- sapply(constraints, is_active_constraint, repr$param)
  type <- rep("Inactive", length(active))

  if ( length(active) == 0 ) {
    count <- c(Number = 0, Active = 0, Inactive = 0)
  } else {
    count <- c(Number = length(constraints),
               Active = sum(active),
               Inactive = sum(!active))
    type[active] <- "Active"
  }
  type <- factor(type, levels = c("Active", "Inactive"))

  details <- data.frame_str(Constraint =
                              value(sapply(constraints, deparse), "character"),
                            Type = type)

  summarized_result("constraints", count, details)
}



is_active_constraint <- function(constraint, parameters) {
  ## TODO: vectorize
  constrained_parameters(list(constraint)) %in% parameters
}



#' @S3method print summarized_constraints
print.summarized_constraints <- function(x, ...) {
  constrs <- split(x$details$Constraint, x$details$Type)

  out <- x$count
  names(out)[1] <- "Constraints:"
  print(out)

  if ( x$count[1] > 0 ) {
    for ( constr in names(constrs) ) {
      out <- paste("    ", constrs[[constr]], collapse = "\n")
      cat("  ", constr, ":\n", out, "\n", sep = "")
    }
  }

  invisible(x)
}



### Summarize data: ##################################################

summarize_data <- function(spec, repr = NULL) {
  stopifnot(is_semspec(spec))
  stopifnot(is.null(repr) | is_semrepr(repr))

  if ( is.null(repr) )
    repr <- semrepr(spec)

  data <- spec$dataset

  if ( is.null(data) ) {
    count <- c(Observations = 0, Variables = 0, Groups = 0)
    details <- data.frame(Variable = value(NULL, "character"),
                          Group = value(NULL, "character"),
                          Level = value(NULL, "character"),
                          Mean = value(NULL, "numeric"),
                          Median = value(NULL, "numeric"),
                          SD = value(NULL, "numeric"),
                          Kurtosis = value(NULL, "numeric"),
                          Skewness = value(NULL, "numeric"),
                          N = value(NULL, "numeric"),
                          "NAs" = value(NULL, "numeric"))

    return(summarized_result("data", count, details))
  }

  ## Manifest variables:
  model_vars <- model_variables(spec$model)
  data_vars <- colnames(spec$dataset)

  manifest <- data.frame(variable = model_vars[model_vars %in% data_vars],
                         stringsAsFactors = FALSE)
  manifest$group <- NA_character_


  ## Grouped manifest variables:
  grouped <- rbind(cbind(variable = repr$lhs, group = repr$group),
                   cbind(variable = repr$rhs, group = repr$group))
  grouped <- na.omit(grouped)
  grouped <- unique(grouped)
  grouped <- grouped[grouped[, "variable"] %in% manifest$variable, , drop = FALSE]

  manifest$group[manifest$variable %in% grouped[, "variable"]] <- grouped[, "group"]


  s1 <- function(x) {
    data.frame(Mean = mean(x, na.rm = TRUE),
               Median = median(x, na.rm = TRUE),
               SD = sd(x, na.rm = TRUE),
               Kurtosis = kurtosis(x, na.rm = TRUE),
               Skewness = skewness(x, na.rm = TRUE),
               N = length(x),
               "NAs" = sum(is.na(x)))
  }

  s2 <- function(x) {
    r <- NULL
    if ( is.na(x["group"]) ) {
      r <- s1(data[[x["variable"]]])
      r$Group <- NA
      r$Level <- NA
      r$Variable <- x["variable"]
    } else {
      r <- aggregate(data[[x["variable"]]],
                     list(group = data[[x["group"]]]), s1,
                     simplify = FALSE)
      r <- cbind(do.call(rbind, r[["x"]]),
                 Level = as.character(r[["group"]]),
                 Group = x["group"],
                 stringsAsFactors = FALSE)
      r$Variable <- x["variable"]
    }
    r
  }

  count <- c(Observations = nrow(data),
             Variables = nrow(manifest),
             Groups = length(unique(na.omit(manifest$group))))
  details <- do.call(rbind, apply(manifest, 1, s2))
  details <- details[c(10, 9, 8, 1:7)]

  summarized_result("data", count, details)
}



#' @S3method print summarized_data
print.summarized_data <- function(x, ...) {
  cat("Data:", x$count["Observations"], "obs. of",
      x$count["Variables"], "variables,",
      x$count["Groups"], "grouping variables\n")

  if ( x$count[1] > 0 ) {
    iprint(x$details, indent = 1, na.print = "", digits = 2, row.names = FALSE)
  }

  invisible(x)
}



######################################################################

value <- function(x, what) {
  if ( is.null(x) | length(x) == 0  )
    return(do.call(what, list(0)))
  x
}



data.frame_str <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}



summarized_result <- function(what, count, details) {
  ret <- list()
  ret$count <- count
  ret$details <- details
  rownames(ret$details) <- NULL

  structure(ret, class = c(sprintf("summarized_%s", what), class(ret)))
}



iprint <- function (object, indent = 4, ...) {
  ## NOTE: shameless copied from package qvcalc!
  zz <- ""
  tc <- textConnection("zz", "w", local = TRUE)
  sink(tc)
  try(print(object, ...))
  sink()
  close(tc)
  indent <- paste(rep(" ", indent), sep = "", collapse = "")
  cat(paste(indent, zz, sep = ""), sep = "\n")
}
