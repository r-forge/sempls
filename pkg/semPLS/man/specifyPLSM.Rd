\name{specifyPLSM}
\Rdversion{1.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{specifyPLSM}
\title{
  %%  ~~function to do ... ~~
  Specification of Path Model
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Creates the specification of a path model used by \code{sempls}. The
  structural and the measurement must be specified in .csv files. The
  paths must be entered in the form of a from-to matrix. The variable in
  the first column represents the source and the second column
  represents the target of each path. 
}
\usage{
specifyPLSM(data, strucmod=NULL, measuremod=NULL, order=c("generic", "alphabetical"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%  \item{strucmod}{
%    %%     ~~Describe \code{file} here~~
%     A from-to \code{character} matrix representing the paths of the
%     structural model.
%  }
%  \item{measuremod}{
%  A from-to \code{character} matrix representing the paths of the measurement model.
%  }
  \item{data}{
    A \code{data.frame} intendend to use for the fitting method, \code{sempls}
  }
  \item{strucmod}{A from-to-matrix representing the structural model. If
    \code{NULL} the path to an .csv file representing the the structural
    model can be entered interactively.}
  \item{measuremod}{A from-to-matrix representing the measurement models. If
    \code{NULL} the path to an .csv file representing the the measurement
    models can be entered interactively.}
  \item{order}{
    %%     ~~Describe \code{file} here~~
     A \code{character} describing how to order the latent variables
     (LVs). If
    \itemize{
      \item \code{"generic"} the LVs will be ordered according to their
      appearance in the causal chain of the structural model  (default).
      \item \code{"alphabetical"} the LVs will be ordered alphabetically.
    }  
  }
}
% \details{
%  %%  ~~ If necessary, more details than the description above ~~
%  The structural and the measurement model are read from .csv files. The
%  paths must be specified as from-to matrizes.
% }
\value{
  %%  ~Describe the value returned
  The object returned is of class \code{plsm} with the elements:
%%  If it is a LIST, use
\item{latent}{A \code{character} vector naming the latent variables.}
\item{manifest}{A \code{character} vector naming the manifest variables.}
\item{strucmod}{Contains only the subset of \code{path} representing the structural model.}
\item{measuremod}{Contains only the subset of \code{path} representing the measurement model.}
\item{D}{The adjacency matrix \eqn{\bm{D}} for the structural model.}
\item{M}{The adjacency matrix \eqn{\bm{M}} for the measurement model.}
\item{blocks}{A \code{list} naming the MVs belonging to each LV's block and telling their measurement mode.}
}

% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
%   %%  ~~who you are~~
%   Armin Graf \email{armin.monecke@gmx.de}
% }

% \note{
%   %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{sempls}}, \code{\link{read.splsm}}, 
}

\examples{
# getting the path to the .csv file representing the inner Model
ptf_Struc <- system.file("ECSIstrucmod.csv", package="semPLS")

# getting the path to the .csv file representing the outer Models
ptf_Meas <- system.file("ECSImeasuremod.csv", package="semPLS")

sm <- as.matrix(read.csv(ptf_Struc))
mm <- as.matrix(read.csv(ptf_Meas))
data(mobi)

ECSI <- specifyPLSM(data=mobi, strucmod=sm, measuremod=mm)
ECSI

### Interactive mode
\dontrun{ECSI <- specifyPLSM(data=mobi)
ECSI}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
