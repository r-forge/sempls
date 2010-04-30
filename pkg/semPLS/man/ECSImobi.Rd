\name{ECSImobi}
\Rdversion{1.1}
\alias{ECSImobi}
\docType{data}
\title{
  Measurement Instrument and the ECSI Model for the Mobile Phone Industry
}
\description{
  European customer satisfaction index (ECSI) adapted to the mobile
  phone market, see Tenenhaus et al. (2005). \code{ECSImobi} includes
  the \code{mobi} data, the from-to-matrizes for structural and
  measurement model and the the representation as \code{plspm} object, \code{ECSI}.
}
\usage{data(ECSImobi)}

\references{
  Tenenhaus, M., V. E. Vinzi, Y.-M. Chatelin, and C. Lauro (2005)
  PLS path modeling.
  Computational Statistics \& Data Analysis 48, 159-205.
}

\author{
Armin Monecke \email{armin.monecke@stat.uni-muenchen.de}
}

\seealso{
  \code{\link{mobi}}, \code{\link{ECSIsm}}, \code{\link{ECSImm}}
}
\examples{
data(ECSImobi)
ecsi <- sempls(model=ECSImobi, data=mobi, E="C")
ecsi
}

\keyword{datasets}

