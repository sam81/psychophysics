geoSE = function(x, na.rm=FALSE){
    if (na.rm==TRUE){
        x = x[is.na(x)==FALSE]
    }
    n = length(x)
    out = exp(sqrt(sum((log(x) - mean(log(x)))^2) / ((n-1)* n)))
    return(out)
}
