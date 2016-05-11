# function to compute the geometric mean
geoMean <- function(x){
  n = length(x)
  m = exp(sum(log(x))/n)
  return(m)
}


#function to compute a weighted geometric mean
# x is the vector containing the values for which to find the mean
# and w is the vector of weights (the same length as x)
wgeoMean <- function(x, w){
  wm = exp(sum(w*log(x))/sum(w))
  return(wm)
}
