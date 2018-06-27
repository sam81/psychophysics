# function to compute the geometric mean
geoMean = function(x, na.rm=FALSE){
    if (na.rm==TRUE){
        x = x[is.na(x)==FALSE]
    }
    n = length(x)
    m = exp(sum(log(x))/n)
    return(m)
}
