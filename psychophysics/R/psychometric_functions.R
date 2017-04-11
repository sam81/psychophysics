erf = function(x) 2 * pnorm(x * sqrt(2)) - 1
erfinv = function (x) qnorm((1 + x)/2)/sqrt(2)

logisticPsy = function(x, alpha, beta, gamma, lambda){

    out = gamma + (1-gamma-lambda) *(1/(1+exp(beta*(alpha-x))))
    return(out)
}

invLogisticPsy = function(p, alpha, beta, gamma, lambda){
    x = alpha - (1/beta)*log((1-gamma-lambda)/(p-gamma) - 1)
    return(x)
}

gaussianPsy = function(x, alpha, beta, gamma, lambda){

    out = gamma+(1-gamma-lambda)*(1+erf((x-alpha)/sqrt(2*beta^2)))/2
    return(out)
}


invGaussianPsy = function(p, alpha, beta, gamma, lambda){

    out = alpha + sqrt(2*beta^2)*erfinv(2*(p-gamma)/(1-gamma-lambda)-1)
    return(out)
}

weibullPsy = function(x, alpha, beta, gamma, lambda){

    out = gamma+(1-gamma-lambda)*(1-exp(-(x/alpha)^beta))
    return(out)
}

invWeibullPsy = function(p, alpha, beta, gamma, lambda){

    out = alpha * ((-log(1-(p-gamma)/(1-gamma-lambda)))^(1/beta))
    return(out)
}

gumbelPsy = function(x, alpha, beta, gamma, lambda){

    out = gamma + (1-gamma-lambda) * (1-exp(-10^(beta*(x-alpha))))
    return(out)
}

invGumbelPsy = function(p, alpha, beta, gamma, lambda){

    out = alpha + (log10(-log(1 - (p-gamma)/(1-gamma-lambda))))/beta
    return(out)
}
