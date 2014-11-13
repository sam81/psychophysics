geoSd <- function(x){
  out = exp(sd(log(x)))
  return(out)
}
  
