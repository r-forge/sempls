\name{misc}
\alias{exogen}
\alias{endogen}
\alias{reflective}
\alias{formative}
\alias{indicators}

\title{Miscellaneous Methods}
\description{
  Miscellaneous methods for \code{plsm} objects.
}
\usage{
exogen(object)
endogen(object)
reflective(object)
formative(object)
indicators(object, LV)
}
\arguments{
  \item{object}{An object of class \code{plsm}.}
  \item{LV}{A character value naming a latent variable contained in the
    model}
}
%\details{
%}
\value{
  \code{exogen} returns the exogenous latent variables contained in a path
  model.

  \code{endogen} returns the endogenous latent variables contained in a path
  model.

  \code{reflective} returns the reflectively measured latent variables
  contained in a path model.

  \code{formative} returns the formatively measured latent variables
  contained in a path model.

  \code{indicators} returns the index or observed variables related to
  the given latent variable.
}

\author{
  Armin Monecke \email{armin.monecke@stat.uni-muenchen.de}
}

\seealso{
  \code{\link{plsm}}, \code{\link{read.splsm}}, \code{\link{sempls}}
}
\examples{
data(ECSImobi)
exogen(ECSImobi)
endogen(ECSImobi)
reflective(ECSImobi)
formative(ECSImobi)
indicators(ECSImobi, "Image")
}

