geoSe <- function(x){
  n <- length(x)
  out = exp(sqrt(sum((log(x) - mean(log(x)))^2) / ((n-1)* n)))
  return(out)
}
