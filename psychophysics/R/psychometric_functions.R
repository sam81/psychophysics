erf = function(x) 2 * pnorm(x * sqrt(2)) - 1
erfinv = function (x) qnorm((1 + x)/2)/sqrt(2)

logisticPsy = function(x, alpha, betax, gammax, lambda){

    out = gammax + (1-gammax-lambda) *(1/(1+exp(betax*(alpha-x))))
    return(out)
}

invLogisticPsy = function(p, alphax, betax, gammax, lambdax){
    x = alphax - (1/betax)*log((1-gammax-lambdax)/(p-gammax) - 1)
    return(x)
}

gaussianPsy = function(x, alphax, betax, gammax, lambdax){

    out = gammax+(1-gammax-lambdax)*(1+erf((x-alphax)/sqrt(2*betax^2)))/2
    return(out)
}


invGaussianPsy = function(p, alphax, betax, gammax, lambdax){

    out = alphax + sqrt(2*betax^2)*erfinv(2*(p-gammax)/(1-gammax-lambdax)-1)
    return(out)
}

weibullPsy = function(x, alphax, betax, gammax, lambdax){

    out = gammax+(1-gammax-lambdax)*(1-exp(-(x/alphax)^betax))
    return(out)
}

invWeibullPsy = function(p, alphax, betax, gammax, lambdax){

    out = alphax * ((-log(1-(p-gammax)/(1-gammax-lambdax)))^(1/betax))
    return(out)
}

gumbelPsy = function(x, alphax, betax, gammax, lambdax){

    out = gammax + (1-gammax-lambdax) * (1-exp(-10^(betax*(x-alphax))))
    return(out)
}

invGumbelPsy = function(p, alphax, betax, gammax, lambdax){

    out = alphax + (log10(-log(1 - (p-gammax)/(1-gammax-lambdax))))/betax
    return(out)
}
