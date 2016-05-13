\name{logaxis}
\alias{logaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pretty axis for log plot}
\description{
  Add an axis with pretty tickmarks for log scaled variable. The
  tickmarks are drawn at powers of 10.
}
\usage{
logaxis(side, base=exp(1), add_ticks=NULL, minor=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{an integer specifying which side of the plot the axis is to
    be drawn on.  The axis is placed as follows: 1=below, 2=left,
    3=above and 4=right}
  \item{base}{the logarithm base used to scale the variable. The default
    is $e$ for the natural logarithm.}
  \item{add_ticks}{additional ticks to be drawn (not powers of 10)}
  \item{minor}{whether minor tick marks should be drawn. Defaults to TRUE}
}
\details{

}

\value{

}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{\code{\link{prevPow10}}}
\examples{
x = seq(0.01, 2*pi, 0.1)
plot(log(x), sin(x), xaxt='n', type='l')
logaxis(1)

#with additional tickmarks
plot(log(x), sin(x), xaxt='n', type='l')
logaxis(1, add_ticks=c(3,5))

#without minor ticks
plot(log(x), sin(x), xaxt='n', type='l')
logaxis(1, minor=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
