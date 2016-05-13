\name{invWeibullPsy}
\alias{invWeibullPsy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the inverse of the weibull psychometric function}
\description{
Compute the inverse of the weibull psychometric function.
}
\usage{
invWeibullPsy(p, alpha, beta, gamma, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{Proportion correct on the psychometric function.}
  \item{alpha}{Mid-point(s) of the psychometric function.}
  \item{beta}{The slope of the psychometric function.}
  \item{gamma}{Lower limit of the psychometric function (guess rate).}
  \item{lambda}{The lapse rate.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Stimulus level at which proportion correct equals `p` for the psychometric function defined by the parameters alpha, beta, gamma, and lambda.
}
\references{Kingdom, F. A. A., & Prins, N. (2010). \emph{Psychophysics:
    A Practical Introduction}. Academic Press.}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%% \note{\code{invWeibullPsy} works only with vectors, not other types of arrays
%% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
invWeibullPsy(0.8, 10, 2, 0.5, 0)
#for multiple values of 'p'
invWeibullPsy(c(0.75, 0.8, 0.9), 10, 2, 0.5, 0)
#for multiple midpoint values
invWeibullPsy(0.8, 1:10, 2, 0.5, 0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
