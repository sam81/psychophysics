
nextPow10Up <- function(val){
    p <- ceiling(log10(val))
    return(p)
  }

nextPow10Down <- function(val){
    p <- floor(log10(val))
    return(p)
  }

x <- seq(0.5, 2.5, 1)
y <- c(10, 100, 1000)
yerr <- c(2, 2, 2)

xlim <- c(0, 3)
mlt <- 2
ylim <- c(min(y)/mlt, max(y)*mlt)


powd <- nextPow10Down(ylim[1])
powup <- nextPow10Up(ylim[2])
majTicks <- seq(powd, powup)

minTicks <- numeric()
for (i in 1:(length(majTicks)-1)){
  minTicks <- c(minTicks, log10(seq(10^majTicks[i], 10^majTicks[i+1], length=10)))
  print(i)
}


X11()
plot.new()
plot.window(xlim=c(0,3), ylim=log10(ylim))
points(x, log10(y))
yl <- log10(y) - log10(yerr)
yu <- log10(y) + log10(yerr)
arrows(x, yl, x, yu, code=3, length=.05, angle=90, col='black')
axis(1)
axis(2, at=majTicks, labels=as.character(10^majTicks))
axis(2, at=minTicks,  labels=FALSE, tcl=-0.25)
box()
