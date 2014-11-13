\name{geoMean}
\alias{geoMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Mean}
\description{
 Compute the geometric mean of a set of values
}
\usage{
geoMean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of positive numbers}
}
\details{
  Values must be positive.
}
\value{
 The geometric mean of the input numerical vector
}
\references{Montgomery, D. C. \& Runger D. C. (2003). \emph{Applied Statistics
and Probability for Engineers}, 3rd Ed. Wiley}
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{geoSd}}, \code{\link{geoSe}}}
\examples{
x <- abs(rnorm(100))
geoMean(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
