# function to compute the geometric mean
geoMean = function(x, na.rm=FALSE){
    if (na.rm==TRUE){
        x = x[is.na(x)==FALSE]
    }
    n = length(x)
    m = exp(sum(log(x))/n)
    return(m)
}


#function to compute a weighted geometric mean
# x is the vector containing the values for which to find the mean
# and w is the vector of weights (the same length as x)
wGeoMean = function(x, w, na.rm=FALSE){
    if (na.rm==TRUE){
        x = x[is.na(x)==FALSE]
        w = w[is.na(x)==FALSE]
    }
    wm = exp(sum(w*log(x))/sum(w))
    return(wm)
}
