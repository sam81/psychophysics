getRMS <- function(signal){
  rms <- sqrt(mean(signal*signal))
  return(rms)
}
