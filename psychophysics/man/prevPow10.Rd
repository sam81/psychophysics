\name{prevPow10}
\alias{prevPow10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Previous Power of 10}
\description{
  Compute the previous power of 10 closest to the given argument.
  In other words, compute the maximum integer exponent to which 10
  should be raised so that 10^x is smaller than the argument.
}
\usage{
prevPow10(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector}
}
\details{

}

\value{
The previous power of 10 closest to the given argument.




}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{nextPow10}}}
\examples{
prevPow10(101)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
