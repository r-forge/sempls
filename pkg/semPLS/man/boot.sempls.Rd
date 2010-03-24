\name{boot.sempls}
\alias{boot.sempls}
\alias{print.bootsempls}
\alias{summary.bootsempls}
\alias{print.summaryBootsempls}

\title{Bootstrap a PLS path model}
\description{
Bootstraps a PLS path model in a \code{sempls} object (as returned by
the \code{\link{sempls}} method).
}

\usage{
boot.sempls(sempls, nboot=200, start=c("ones", "old"), method=
            c("ConstructLevelChanges", "IndividualSignChanges",
            "Standard"), verbose=TRUE, \ldots)

\method{print}{bootsempls}(x, digits=getOption("digits"), \ldots)

\method{summary}{bootsempls}(object, type=c("perc", "bca", "norm", "basic", "none"),
        level=0.95, \ldots)

\method{print}{summaryBootsempls}(x, digits = getOption("digits"), \ldots)
}

\arguments{
  \item{sempls}{An object of class \code{sempls} as returned by the method \code{sempls}.}
  \item{nboot}{The number of bootstrap replications; the default is \code{200}.}
  \item{start}{A \code{character} value telling, how to initialise the
    outer weights. If
    \itemize{
     \item \code{start="ones"}, then the outer weights for each block are initialised by ones.
     \item \code{start="old"}, the final outer weights taken from the
     \code{sempls} object are used for initialisation.
    }
  }
  \item{method}{ A \code{character} value, which can take the values:
    \itemize{
      \item \code{"ConstructLevelChanges"} (default): The vector of
      loadings for each LV in each resample is compared to the corresponding
      vector of loadings in the original sample. The signs of the weights,
      and consequently the signs of the loadings, are reversed if the
      absolute value of the sum differences between estimated loadings
      obtained from the original sample and the estimated loadings obtained
      from the resample is bigger than the absolute value of the sum sums of
      the latter, see Tenenhaus et al (2005).
      \item \code{"IndividualSignChanges"}: not implemented yet.
      \item \code{"Standard"}: No compensation for sign changes of
      resampled statistics.
    }
  }
  \item{verbose}{A \code{logical} indicating, whether progress of bootstrap shall
  be displayed.}
  \item{x, object}{An object of class \code{bootsempls}.}
  \item{digits}{Controls the number of digits to print.}
  \item{type}{Type of bootstrapped confidence intervals to compute; the
    default is \code{"perc"} (percentile); see
    \code{\link[boot]{boot.ci}} for details.}
  \item{level}{Level for confidence intervals; default is \code{0.95}.}
  \item{\ldots}{Arguments to be passed down to other methods.}
}

\details{
  \code{boot.sempls} implements the nonparametric bootstrap, assuming an
  independent random sample. Convergence failures in the bootstrap resamples
  are discarded (and a warning printed); \eqn{10} consecutive convergence failures
  result in an error. You can use the \code{\link[boot]{boot}} function
  in the boot package for more complex sampling schemes and additional options.
}

\value{
  \code{boot.sempls} returns an object of class \code{bootsempls}, which inherits
    from class \code{boot}, supported by the \code{boot} package. The returned
    object contains the following components:
  \item{t0}{the estimated parameters in the model fit to the original data set.}
  \item{t}{a matrix containing the bootstrapped estimates, one bootstrap
    replication per row.}
  \item{data}{the data frame containing the data to which the model was fit.}
  \item{seed}{the value of \code{.Random.seed} when \code{boot.sempls} was called.}
  \item{statistic}{the function used to produce the bootstrap replications;
    this is always the local function \code{refit} from \code{boot.sempls}.}
  \item{sim}{always set to \code{"ordinary"}; see the documentation for the
    \code{\link[boot]{boot}} function.}
  \item{stype}{always set to \code{"i"}; see the documentation for the
    \code{\link[boot]{boot}} function.}
  \item{call}{the call of the \code{boot.sem} function.}
  \item{tryErrorIndices}{Contains the indices for each resample, which
  returned \code{try-error}.}
  \item{clcIndex}{When the method \code{ConstructLevelChanges} is used,
  it stores the indices of the blocks of MVs for which a sign change occured.}
}

% \author{
%   Armin Monecke \email{armin.monecke@gmx.de}
% }

\seealso{\code{\link[boot]{boot}}, \code{\link[sem]{boot.sem}}}

\references{
  Tenenhaus, M., V. E. Vinzi, Y.-M. Chatelin, and C. Lauro (2005)
  PLS path modeling. Computational Statistics \& Data Analysis 48,
  159-205.
}

% \examples{
%     \dontrun{
%     }
% }

% \keyword{htest}
% \keyword{models}