\name{fliplr}
\alias{fliplr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Flip a vector left to right}
\description{
Flip a vector (numerical, character or logical), left to right, so that
the last element becomes the first, the last but one becomes the second,
and so on.
}
\usage{
fliplr(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the vector to be flipped}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
a vector of the same mode as the input vector, flipped in the left right direction
}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
\note{\code{fliplr} works only with vectors, not other types of arrays
}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- 1:10
y <- fliplr(x)
print(y)

x <- c('a', 'b', 'c', 'd', 'e')
y <- fliplr(x)
print(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
