fliplr <- function(x){
  n <- length(x)
  if (mode(x) == 'numeric'){
    y <- numeric(n)
  } else if (mode(x) == 'character'){
    y <- character(n)
  } else if (mode(x) == 'logical'){
    y <- logical(n)
  }
  
  for (i in 1:n){
    y[i] <- x[(1+n-i)]
  }
  return(y)
}
