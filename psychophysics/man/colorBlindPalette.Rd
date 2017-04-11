\name{colorBlindPalette}
\alias{colorBlindPalette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A colorblind friendly color palette}
\description{
Return a color palette which is colorblind friendly.
}
\usage{
colorBlindPalette(n=9, alpha=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{the number of colors in the palette. The maximum is 9.}
  \item{alpha}{opacity of the colors. A value between 0, and
    1. The opacity decreases as alpha decreases.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Color palette as a vector of hexadecimal strings
}

\references{http://jfly.iam.u-tokyo.ac.jp/color/}
%\references{ ~put references to the literature/web site here ~ }
\author{Samuele Carcagno}
%\note{
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
plot(1:9, col=colorBlindPalette(), cex=2, pch=16)
plot(1:9, col=colorBlindPalette(alpha=0.5), cex=2, pch=16)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
