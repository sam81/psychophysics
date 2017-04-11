
logistic_likelihood = function(p, lev, nCorr, nIncorr, gammax){
    mu = p[1]
    sigma = p[2]
    lambda=p[3]
    pr = logisticPsy(lev, mu, sigma, gammax, lambda)
    pr[pr < .Machine$double.eps] <- .Machine$double.eps
    pr[pr > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps
    -sum(nCorr*log(pr)+nIncorr*log(1-pr))
}

fitLogisticPsyML = function(levs, resp, guessRate, xscale="linear", minSlope=0.0001, minMidpoint=NULL, maxMidpoint=NULL, maxSlope=10, minLapse=0, maxLapse=0.45, initMidpoint=NULL, initSlope=NULL, initLapse=NULL){

    if (xscale == "log"){
        levs = log(levs)
        if (is.null(minMidpoint) == FALSE){
            minMidpoint = log(minMidpoint)
        }
        if (is.null(maxMidpoint) == FALSE){
            maxMidpoint = log(maxMidpoint)
        }
        if (is.null(initMidpoint) == FALSE){
            initMidpoint = log(initMidpoint)
        }
    }
    pc = tapply(resp, levs, mean)
    lev = as.numeric(dimnames(pc)[[1]])
    foo = rep(1, length(resp))
    nTrials = tapply(foo, levs, sum)
    nCorr = tapply(resp, levs, sum)
    nIncorr = nTrials-nCorr

    if (is.null(minMidpoint)){
        minMidpoint = min(levs)
    }

    if (is.null(maxMidpoint)){
        maxMidpoint = max(levs)
    }

    if (is.null(initMidpoint)){
        initMidpoint = (minMidpoint+maxMidpoint)/2
    }
    if (is.null(initSlope)){
        initSlope = (minSlope+maxSlope)/2
    }
    if (is.null(initLapse)){
        initLapse = (minLapse+maxLapse)/2
    }
    
    thisFitRes = optim(par=c(initMidpoint, initSlope, initLapse), logistic_likelihood, lev=lev, nCorr=nCorr, nIncorr=nIncorr, gammax=guessRate, lower=c(minMidpoint, minSlope, minLapse), upper=c(maxMidpoint, maxSlope, maxLapse), method="L-BFGS-B")

    mdpnt = thisFitRes$par[1]
    slope = thisFitRes$par[2]
    lapse = thisFitRes$par[3]

    if (xscale=="log"){
        mdpnt = exp(mdpnt)
    }

    res = list(midpoint=mdpnt, slope=slope, lapse=lapse)
    return(res)

}
