library(optparse)
 
option_list = list(
  make_option(c("-m", "--message"), type="character", default=NULL, 
              help="git commit message", metavar="character")
); 
 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$message)){
  print_help(opt_parser)
  stop("Please supply git commit message argument.", call.=FALSE)
}

lns = readLines("psychophysics/DESCRIPTION")

bump = "patch"
for (i in 1:length(lns)){
    thisLine = lns[i]
    ## bump version number
    if (strsplit(thisLine, ":")[[1]][1] == "Version"){
        prevVer = strsplit(thisLine, ":")[[1]][2]
        newVer = as.numeric(unlist(strsplit(prevVer, '[.]',)))

        if (bump == "major"){
            newVer[1] = newVer[1] + 1
        } else if (bump == "minor"){
            newVer[2] = newVer[2] + 1
        } else if  (bump == "patch"){
            newVer[3] = newVer[3] + 1
        }

        newVer = paste(as.character(newVer[1]), as.character(newVer[2]), as.character(newVer[3]), sep=".")
        lns[i] = paste("Version: ", newVer, sep="")
        
    }

    ## update date
    if (strsplit(thisLine, ":")[[1]][1] == "Date"){
        lns[i] = paste("Date: ", Sys.Date(), sep="")
        
    }

    
}

writeLines(lns, "psychophysics/DESCRIPTION")

##set git tag
gittag = newVer
cmd = paste0("git commit -a -m '", opt$message, "'")
system(cmd)
cmdTag = paste0("git tag -a '", gittag, "'", " -m '", gittag, "'")
system(cmdTag)
