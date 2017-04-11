logaxis = function(side, base=exp(1), add_ticks=NULL, minor=TRUE, labels=TRUE, ...){
    if (side == 1 || side == 3){
        lims = par("usr")[1:2]
    } else if (side == 2 || side == 4){
        lims = par("usr")[3:4]
    }
    lims = base^lims
    
    minPow = prevPow10(lims[1])
    maxPow = nextPow10(lims[2])
    powSeq = seq(minPow, maxPow)
    axSeq = 10^powSeq
    if (is.null(add_ticks) == FALSE){
        extendedAxSeq = c(axSeq, add_ticks)
    } else  {
        extendedAxSeq = axSeq
    }
    if (labels == TRUE){
        axis(side, at=log(extendedAxSeq, base=base), labels=as.character(extendedAxSeq), ...)
    } else {
        axis(side, at=log(extendedAxSeq, base=base), labels=labels, ...)
    }
    if (minor == TRUE){
        for (i in 1:length(axSeq)){
            bb = (1:10)/10; a = (bb*10^powSeq[i]);   axis(side, at=log(a, base=base), tcl=-0.25, labels=F)
        }
    }
}
 
