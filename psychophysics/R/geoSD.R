geoSD = function(x, na.rm=FALSE){
    if (na.rm==TRUE){
        x = x[is.na(x)==FALSE]
    }
    out = exp(sd(log(x)))
    return(out)
}
  
