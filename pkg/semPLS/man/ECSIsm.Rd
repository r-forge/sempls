\name{ECSIsm}
\docType{data}
\alias{ECSIsm}
\title{ECSI Structural Model for the Mobile Phone Industry}
\description{This data provides the structural model for the european
  customer satisfaction index (ECSI) adapted to the mobile market, see
  Tenenhaus et al. (2005).
}
\usage{
data(ECSImobi)
data(ECSIsm)
}
\format{
  A from-to-matrix (\eqn{24} x \eqn{2}) representing the directed edges connecting the
  latent variables.
  \describe{
    \item{\code{from}}{The tail of an edge}
    \item{\code{to}}{The head of an edge}
  }
}


\references{
  Tenenhaus, M., V. E. Vinzi, Y.-M. Chatelin, and C. Lauro (2005)
  PLS path modeling.
  Computational Statistics \& Data Analysis 48, 159-205.
}

\seealso{
  \code{\link{ECSImobi}}, \code{\link{mobi}}, \code{\link{ECSImm}}
}
\examples{
data(ECSImobi)
ecsi <- sempls(model=ECSI, data=mobi, E="C")
ecsi
}

\keyword{datasets}
