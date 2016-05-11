\name{geoSD}
\alias{geoSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Standard Deviation}
\description{
Compute the geometric standard deviation of a set of values
}
\usage{
geoSD(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of positive values}
}
\details{

}

\value{
The geometric standard deviation of the input data




}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{geoMean}}, \code{\link{geoSE}}}
\examples{
x <- abs(rnorm(100))
geoSD(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
