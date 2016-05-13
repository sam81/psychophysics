\name{gumbelPsy}
\alias{gumbelPsy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the psychometric gumbel function}
\description{
Compute the psychometric gumbel function.
}
\usage{
gumbelPsy(x, alpha, beta, gamma, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Stimulus level(s).}
  \item{alpha}{Mid-point(s) of the psychometric function.}
  \item{beta}{The slope of the psychometric function.}
  \item{gamma}{Lower limit of the psychometric function (guess rate).}
  \item{lambda}{The lapse rate.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
The proportion of correct responses for the psychometric function
defined by the parameters alpha, beta, gamma, and lambda, evaluated at the stimulus level x.
}
\references{Kingdom, F. A. A., & Prins, N. (2010). \emph{Psychophysics: A Practical Introduction}. Academic Press.}
\author{Samuele Carcagno}
%% \note{\code{gumbelPsy} works only with vectors, not other types of arrays
%% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
gumbelPsy(10, 10, 2, 0.5, 0)
#for more than one stimulus level
gumbelPsy(5:15, 10, 2, 0.5, 0)
#for more than one midpointl
gumbelPsy(10, 5:15, 2, 0.5, 0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
