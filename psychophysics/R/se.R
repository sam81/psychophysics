se <- function(x, na.rm=FALSE){
    if (na.rm == TRUE){
        x = x[is.nan(x)==FALSE]
    }
    n <- length(x)
    out <- sd(x) / sqrt(n)
    return(out)
}
