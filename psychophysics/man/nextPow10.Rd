\name{nextPow10}
\alias{nextPow10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Next Power of 10}
\description{
  Compute the next power of 10 closest to the given argument.
  In other words, compute the minimum integer exponent to which 10
  should be raised so that 10^x is greater than the argument.
}
\usage{
nextPow10(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector}
}
%% \details{

%% }

\value{
The next power of 10 closest to the given argument.




}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{prevPow10}}}
\examples{
nextPow10(101)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
