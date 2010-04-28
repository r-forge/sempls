\name{rSquared}
\alias{rSquared}
\alias{rSquared.sempls}
\title{R-squared}
\description{
  Computes the R-squared values for \code{sempls} objects.
}
\usage{
rSquared(object)

\method{rSquared}{sempls}(object, na.rm=FALSE)
}
\arguments{
  \item{object}{An object of class \code{sempls}.
  }
  \item{na.rm}{logical. Should missing values be removed?
  }
}
\value{
  A matrix containing R-squared values for each endogeneous latent variable.
}

\seealso{
  \code{\link{sempls}}, \code{\link{pathDiagram}}
}
