\name{se}
\alias{se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standard error of the mean}
\description{
This function computes the standard error of the mean of the values in
     'x'.  If 'na.rm' is 'TRUE' then missing values are removed before
     computation proceeds.
}
\usage{
se(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector}
  \item{na.rm}{a logical value indicating whether ‘NA’ values should be stripped before the computation proceeds.}
}
%% \details{


%% }
\value{
 The standard error of the mean of the input data
}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{geoSE}}}
\examples{
x = rnorm(100)
se(x)
}
