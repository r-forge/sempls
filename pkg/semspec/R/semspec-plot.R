#' @include semrepr.R
#' @include semfit.R
{}



#' @S3method plot semspec
plot.semspec <- function(x, y = NULL, ...) {
  vars <- summary(x)$variables$details
  manifest <- subset(vars, Type == "Manifest")$Variable

  qgraph(as_sem_syntax(x), layout = "tree", manifest = manifest)
}

