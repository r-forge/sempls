\name{ic}
\alias{ic}
\alias{ic.sempls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Information Criteria for PLS Path Models
}
\description{
The function computes a variety of information criteria for PLS path models.
}
\usage{
ic(object, LV, criteria, ...)

\method{ic}{sempls}(object, LV, criteria, \ldots)
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
  \item{\dots}{
    Argument to pass down to other methods. Currently not used.
}
}
%% \details{
%% %%  ~~ To Do: Pratyush ~~
%% }
\value{
The function returns a \code{numeric} vector for the calculated
information criteria of the model of the focal component (latent
variable). 
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
\code{\link{ic_exhaustive}}, \code{\link[semPLS]{sempls}}
}
\examples{
library("semPLS")
data(ECSImobi)
ecsi <- sempls(ECSImobi, mobi, wscheme="centroid")


### ic - information criteria

## All criteria
ic(ecsi, LV = "Satisfaction")
ic(ecsi, LV = "Loyalty")

## Final Prediction Error
ic(ecsi, LV = "Satisfaction", criteria = "FPE")


## Adjusted R-sq
ic(ecsi, LV = "Satisfaction", criteria = "AdjRsq")


## AIC
ic(ecsi, LV = "Satisfaction", criteria = "AIC")
ic(ecsi, LV = "Satisfaction", criteria = "AIC2")

## Unbiased AIC
ic(ecsi, LV = "Satisfaction", criteria = "AICu")

## Corrected AIC
ic(ecsi, LV = "Satisfaction", criteria = "AICc")

## Bayesian IC
ic(ecsi, LV = "Satisfaction", criteria = "BIC")
ic(ecsi, LV = "Satisfaction", criteria = "BIC2")

## Hannan Quinn
ic(ecsi, LV = "Satisfaction", criteria = "HQ")

## Corrected Hannan Quinn
ic(ecsi, LV = "Satisfaction", criteria = "HQc")

## CP
ic(ecsi, LV = "Satisfaction", criteria = "Cp")

## GM
ic(ecsi, LV = "Satisfaction", criteria = "GM")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{PLS}
\keyword{strutural equation modeling}
\keyword{information criteria}
\keyword{path model}

