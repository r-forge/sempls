\name{ic_exhaustive}
\alias{ic_exhaustive}
\alias{ic_exhaustive.sempls}
\alias{print.ic_exhaustive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exhaustive Information Criteria for PLS Path Models
}
\description{
The function computes a variety of information criteria for exhaustive PLS path models.
}
\usage{
ic_exhaustive(object, LV, criteria, ...)

\method{ic_exhaustive}{sempls}(object, LV, criteria, \ldots)

\method{print}{ic_exhaustive}(x, minlength, digits, \ldots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    an object of class \code{\link[semPLS]{sempls}}
}
  \item{LV}{
    the focal component (latent variable)
}
  \item{criteria}{
    A \code{character} vector naming the information criteria to
    calculate, see details.
    %%     ~~Describe \code{criteria} here~~
}

\item{x}{
  object of class \code{ic_exhaustive}
}
\item{minlength}{
  the minimum length of the abbreviations
}
\item{digits}{
  specifies the minimum number of significant digits to be printed in
  values, see \code{\link{print.default}}
}
  \item{\dots}{
    Arguments to pass down to other methods.
}
}
%% \details{
%% %%  ~~ To Do: Pratyush ~~
%% }
\value{
%%  If it is a LIST, use
 \item{ic}{A \code{numeric} matrix containing the information
 criteria in the columns and models in the rows with respect to the
 focal component (latent variable).}
 \item{all_fits}{A \code{list} of all fitted models resulting from an
 exhaustive search for the focal component (latent variable)}
}
%% \references{
%% %% ~ To Do: Pratyush ~
%% }
%% \author{
%% %%  ~~who you are~~
%% }
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ic}}, \code{\link[semPLS]{sempls}}
}
\examples{
library("semPLS")
data(ECSImobi)
ecsi <- sempls(ECSImobi, mobi, wscheme="centroid")

### ic - information criteria
## All criteria for 'all' models
ic_exhaustive(ecsi, LV = "Satisfaction")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{PLS}
\keyword{strutural equation modeling}
\keyword{information criteria}
\keyword{path model}
\keyword{exhaustive}

