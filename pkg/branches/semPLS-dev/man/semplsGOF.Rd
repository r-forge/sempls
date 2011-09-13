\name{semplsGOF}
\Rdversion{1.1}
\alias{rSquared}
\alias{rSquared.sempls}
\alias{print.rSquared}
\alias{qSquared}
\alias{qSquared.sempls}
\alias{print.qSquared}


\title{
Goodness of fit measures for pls path models
}
\description{
  More detailed description.
}
\usage{
rSquared(object, \ldots)
\method{rSquared}{sempls}(object, na.rm=FALSE, \dots)
\method{print}{rSquared}(x, na.print=".", digits=2, \ldots)

qSquared(object, \ldots)
\method{qSquared}{sempls}(object, d=NULL, impfun, dlines=TRUE,
         total=FALSE, \dots)
\method{print}{qSquared}(x, na.print=".", digits=2, \ldots)
}
\arguments{
  \item{object}{
    An object of class \code{sempls}.
  }
  \item{d}{
    A \code{numeric} value for the omission distance. Thus choosing
    \eqn{d=N-1}, where \eqn{N} is the number of complete observations,
    is leaving one out cross validation. This is done when \code{d}
    takes its default value \code{NULL}.
  }
  \item{impfun}{
    An user specified function to impute missing values.
  }
  \item{dlines}{
    If \code{TRUE} the same observations are deleted for a whole block
    of MVs, else each \code{d}th , counting from top left to bottom
    right, observation is deleted.
  }
  \item{total}{
    If \code{total=TRUE} total effects are used instead of path
    coefficients to calculate the predictions.
  }
  \item{na.rm}{
    If \code{na.rm=TRUE} observation with missing values are discarded 
    before analysis.
  }
  \item{x}{
    An object of the according class.
  }
  \item{na.print}{
    A \code{character} substituting values not to be printed.
  }
  \item{digits}{
    minimal number of _significant_ digits, see \code{\link{print.default}}.
  }
  \item{\dots}{
    Arguments to be passed down.
  }
}

\value{
  Most GOF methods return a column vector with the names of the variables as
  rows and the respective measure as column.
}

\author{
  Armin Monecke \email{armin.monecke@stat.uni-muenchen.de}
}

\seealso{
   \code{\link{sempls}}, \code{\link{plsLoadings}}
}
\examples{
data(ECSImobi)
ecsi <- sempls(model=ECSImobi, data=mobi, E="C")

### R-squared
rSquared(ecsi)

### Q-squared with omission distance d=4
qSquared(ecsi, d=4)

## get loadings and check for discriminant validity
(l <- plsLoadings(ecsi))
# outer loadings
print(l, type="outer", digits=2)
# outer loadings greater than 0.5
print(l,type="outer", cutoff=0.5, digits=2)
# cross loadings greater than 0.5
print(ecsi, type="cross", cutoff=0.5, digits=2)
}
