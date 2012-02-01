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
  ret$variables <- summarize_variables(object, repr)
  ret$parameters <- summarize_parameters(object, repr)
  ret$constraints <- summarize_constraints(object, repr)
  ret$data <- summarize_data(object, repr)
  ret$df <- model_df(ret$variables$count, ret$parameters$count)


  structure(ret, class = c("summary.semspec", class(ret)))
}



#' @S3method print summary.semspec
print.summary.semspec <- function(x, details = FALSE, ...) {
  cat("Structural equation model specification\n")
  for ( part in x ) {
    print(part, details = details, ...)
    cat("\n")
  }
}





### Degrees of freedom: ##############################################

summarize_df <- function(object, repr) {
  stopifnot(is_semspec(spec))
  stopifnot(is_semrepr(repr))

  vars <- summarize_variables(object, repr)
  params <- summarize_parameters(object, repr)

  model_df(vars$count, params$counts)
}



model_df <- function(variables, parameters) {
  manifest <- variables["Manifest"]
  free <- parameters["Free"] + parameters["Restricted"]

  t <- free
  p <- (manifest/2) * (manifest + 1)

  count <- c(DF = unname(p - t))
  details <- data.frame(estimates = unname(t),
                        empiricals = unname(p))

  summarized_result("Degrees of freedom", count, details)
}



### Summarize variables: #############################################

summarize_variables <- function(spec, repr) {
  stopifnot(is_semspec(spec))
  stopifnot(is_semrepr(repr))

  model_vars <- model_variables(spec$model)
  data_vars <- colnames(spec$dataset)

  manifest <- model_vars %in% data_vars

  type <- rep("Latent", length(model_vars))
  type[manifest] <- "Manifest"
  type <- factor(type, levels = c("Latent", "Manifest"))

  count <- c(Number = length(model_vars), table(type))
  details <- data.frame_str(Variable = model_vars, Type = type)

  summarized_result("Variables", count, details)
}



model_variables <- function(model) {
  vars <- function(x) {
    group <- find_group(x)
    if ( !is.null(group) ) {
      x <- remove_group(x)
    }
    all.vars(x)
  }

  model$group <- NULL
  unname(unlist(sapply(model, function(x) lapply(x, vars))))
}



### Summarize parameters: ############################################

summarize_parameters <- function(spec, repr) {
  stopifnot(is_semspec(spec))
  stopifnot(is_semrepr(repr))

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

  summarized_result("Parameters", count, details)
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



### Summarize constraints: ###########################################

summarize_constraints <- function(spec, repr) {
  stopifnot(is_semspec(spec))
  stopifnot(is_semrepr(repr))

  constraints <- spec$constraints
  data <- spec$dataset

  active <- sapply(constraints, is_active_constraint, repr$param)

  count <- c(Number = length(constraints),
             Active = if ( length(active) == 0 ) 0 else sum(active),
             Inactive = if ( length(active) == 0 ) 0 else sum(!active))

  details <- data.frame_str(Constraint =
                              value(sapply(constraints, deparse), "character"),
                            Active = value(active, "logical"))

  summarized_result("Constraints", count, details)
}



is_active_constraint <- function(constraint, parameters) {
  ## TODO: vectorize
  constrained_parameters(list(constraint)) %in% parameters
}



### Summarize data: ##################################################

summarize_data <- function(spec, repr) {
  stopifnot(is_semspec(spec))
  stopifnot(is_semrepr(repr))

  data <- spec$dataset

  if ( is.null(data) ) {
    count <- c(Observations = 0, Variables = 0, Groups = 0)
    details <- data.frame(Mean = value(NULL, "numeric"),
                          Median = value(NULL, "numeric"),
                          SD = value(NULL, "numeric"),
                          Kurtosis = value(NULL, "numeric"),
                          Skewness = value(NULL, "numeric"),
                          N = value(NULL, "numeric"),
                          "NA" = value(NULL, "numeric"),
                          Group = value(NULL, "character"),
                          Variable = value(NULL, "character"))

    return(summarized_result("Data", count, details))
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
  grouped <- grouped[grouped[, "variable"] %in% manifest$variable, ]

  manifest$group[manifest$variable %in% grouped[, "variable"]] <- grouped[, "group"]


  s1 <- function(x) {
    data.frame(Mean = mean(x, na.rm = TRUE),
               Median = median(x, na.rm = TRUE),
               SD = sd(x, na.rm = TRUE),
               Kurtosis = kurtosis(x, na.rm = TRUE),
               Skewness = skewness(x, na.rm = TRUE),
               N = length(x),
               "NA" = sum(is.na(x)))
  }

  s2 <- function(x) {
    r <- NULL
    if ( is.na(x["group"]) ) {
      r <- s1(data[[x["variable"]]])
      r$Group <- NA
      r$Variable <- x["variable"]
    } else {
      r <- aggregate(data[[x["variable"]]],
                     list(group = data[[x["group"]]]), s1,
                     simplify = FALSE)
      r <- cbind(do.call(rbind, r[["x"]]),
                 Group = as.character(r[["group"]]),
                 stringsAsFactors = FALSE)
      r$Variable <- x["variable"]
    }
    r
  }

  count <- c(Observations = nrow(data),
             Variables = nrow(manifest),
             Groups = length(unique(na.omit(manifest$group))))
  details <- do.call(rbind, apply(manifest, 1, s2))

  summarized_result("Data", count, details)
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
  ret$counts <- count
  ret$details <- details
  rownames(ret$details) <- NULL

  structure(ret, what = what,
            class = c("summarized_result", class(ret)))
}



#' @S3method print summarized_result
print.summarized_result <- function(x, details = FALSE, ...) {
  cat(attr(x, "what"), ":\n", sep = "")
  print(x$count)
  if ( details ) {
    cat("\n")
    print(x$details)
  }
}

