#' @include semrepr.R
#' @include semfit.R
{}



#' @S3method plot semspec
plot.semspec <- function(x, y = NULL, ...) {
  vars <- summary(x)$variables$details
  manifest <- subset(vars, Type == "Manifest")$Variable

  sem_syntax2semmod <- function(sem_syntax, ...){
    tmp=file()
    cat(sem_syntax, file=tmp)
    sem_model <- specifyEquations(file = tmp, ...)
    close(tmp)
    return(sem_model)
  }

  sem_model <- sem_syntax2semmod(as_sem_syntax(x), ...)
  
  qgraph(sem_model, layout = "tree", manifest = manifest)
}

