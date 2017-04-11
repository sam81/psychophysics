colorBlindPalette = function(n=9, alpha=1){

#taken from http://www.ucl.ac.uk/~zctpep9/Archived%20webpages/Cookbook%20for%20R%20%C2%BB%20Colors%20%28ggplot2%29.htm
#http://jfly.iam.u-tokyo.ac.jp/color/    

    cbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")[1:n]

    cbPalette = adjustcolor(cbPalette, alpha.f=alpha)
    
    return(cbPalette)
}

