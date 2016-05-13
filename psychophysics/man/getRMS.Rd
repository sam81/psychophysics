\name{getRMS}
\alias{getRMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the RMS of a signal }
\description{
  Given a signal, compute its root mean square (RMS) value.
}
\usage{
getRMS(signal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{a vector of values representing the signal}
}
\details{
  The RMS is calculated by computing the square root of the mean of the
  signal values squared.
}
\value{
  RMS value of the signal.
}
\references{Hartmann, W. M. (1998) \emph{Signals, sound and sensation}. New York: Springer.}
\author{Samuele Carcagno}

\seealso{\code{\link{adjustRMS}} }
\examples{
# generate random signal
x = rnorm(100)
# compute RMS
getRMS(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
