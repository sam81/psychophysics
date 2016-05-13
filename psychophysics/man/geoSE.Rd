\name{geoSE}
\alias{geoSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Standard Error of the Geometric Mean}
\description{
Compute the geometric standard error of the geometric mean of a set of values
}
\usage{
geoSE(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector of positive values}
}
\details{


}
\value{
 The geometric standard error of the geometric mean of the input data
}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{geoSD}}, \code{\link{geoMean}}}
\examples{
x = abs(rnorm(100))
geoSE(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
