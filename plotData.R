#!/usr/bin/Rscript

################################################################################
## help, error & cia

thisScript <- "plotData.R"

error <- function(msgs="") {
  cat('[', thisScript, '] ERROR: ', msgs, '\n', sep="")
  quit(save="no")
}

warning <- function(msgs="") {
  cat('[', thisScript, '] WARNING: ', msgs, '\n', sep="")
}

helpText <- function(){
  cat('Syntax 
  ./$0 [[--args] arguments]
    "--args" is to separate the R options form the script arguments
    lists are of the form "element1 element2 ..."
Arguments:
  --tests=list - tests to plot; default: all; available:',knownTests,'all
  --protos=list - protocols to plot; default: all; available:',knownProtocols,'all
  --colours=list - protocol colours (pairs with --proto); default:',protoColours,'
  --dataext=character - data files extension; default:',dataext,'
  --indir=path/to/dir - input directori, where data files are located
  --outdir=path/to/dir - output dir, where *.pdf are saved
  --decorate=TRUE|FALSE - add legends; default: TRUE (TODO: extend to other fields? or remove opt?)
  --help   - print this text
Example:
  ./$0 [--args --tests="netperf top" --protos="olsr1 bmx"] \n')
}


################################################################################
## ARGUMENTS

knownTests <- c("netperf", "top", "tcpdump") ## used to define testsNames
knownProtocols <- c("olsr1", "bmx", "batadv", "olsr2", "babel") ## used to define protocolNames
protoColours <- c( "blue", "green", "red",  "maroon1", "black") ## overwritten if param passed
dataext <- ".data" ## overwritten if param passed
indir <- "." ## overwritten if param passed
outdir <- "." ## overwritten if param passed
decorate <- TRUE

args <- commandArgs(trailingOnly = TRUE)
if("--help" %in% args) { helpText(); q(save = "no") }

## Parse arguments (we expect the form --arg=value)
argsDF <- as.data.frame(do.call("rbind", strsplit(sub("^--", "", args), "=")))
argsL <- as.list(as.character(argsDF$V2))
names(argsL) <- argsDF$V1

#### default values
if(is.null(argsL$tests)) { testsNames <- "all"
} else {
  testsNames <- strsplit(argsL$tests, " ")[[1]] ## [[1]] to extract the first element of the list, which is a vector
}
if(FALSE %in% { testsNames %in% c(knownTests, "all") }){ print("ERROR: Unknow test."); helpText(); quit(save = "no") }
if("all" %in% testsNames) { testsNames <- knownTests }

## protos
if(is.null(argsL$protos)) { protosNames <- "all"
} else {
  protosNames <- strsplit(argsL$proto, " ")[[1]]
}
if(FALSE %in% { protosNames %in% c(knownProtocols, "all") }){ print("WARNING: Unknow protocol.")}
if("all" %in% protosNames) { protosNames <- knownProtocols }

## colours
if(!is.null(argsL$colours)) { protoColours <- replace(protoColours, 1:length(strsplit(argsL$colours, " ")[[1]]), strsplit(argsL$colours, " ")[[1]]) }
if(FALSE %in% { protoColours %in% colours() }){ print("ERROR: Unknow colour. See colours() in R."); quit(save = "no") }

if(!is.null(argsL$dataext)) { dataext <- argsL$dataext}
if(!is.null(argsL$indir)) { indir <- argsL$indir}
if(!is.null(argsL$outdir)) { outdir <- argsL$outdir}

if(!is.null(argsL$decorate)) { decorate <- argsL$decorate}


################################################################################
## FUCNTIONS

## strings convolution
strConv <- function(vect1, vect2=protosNames){
  conv = NULL
  for(i in vect2){  for(j in vect1){ conv <- c(conv, paste(i, j, sep="")) }  }
  conv
}


################################################################################
## PLOTS

if("netperf" %in% testsNames) {
  test <- "netperf"
  dataFile <- paste(indir, "/", test, dataext, sep = "")
  if ( !file.exists(dataFile) ) { warning(msg=paste("Not plotting. File does not exist: ", dataFile, sep = ""))
  } else {
    ##  [1] "olsr1TP"  "olsr1T"   "bmxTP"    "bmxT"     etc
    data <- read.table(dataFile, header = TRUE)
    
    if (nrow(data)==0) { warning(msg=paste("Not plotting. Empty file: ", dataFile, sep = ""))
    } else {
      ## roundsText<-c("round1","round2",...)
      roundsText = NULL; for (i in 1:length(data[,1])) {roundsText <- c(roundsText, paste("round", i, sep=""))}
      
      ## THROUGHPUT
      id <- "TP"
      dataNames=strConv(id)
      pdf(file = paste(outdir, "/", test, "_", id, ".pdf", sep = ""))
      ## empty column to make room for legend; as a consequence xlab must also be faked
      barplot(as.matrix(cbind(data[,dataNames],NA)), main = "netperf test", names.arg = c(protosNames,""), ylab = "Throughput [KB/s]", legend.text = roundsText, args.legend = list("topleft",bty = "n",cex=0.8), beside = TRUE) ## todo fix ylim shortness
      
      for(i in 1:length(dataNames)){lines(par('usr')[1:2], rep(mean(data[,dataNames[i]],na.rm = TRUE),2), type="l", lty = 2, col=protoColours[i])} ## TODO fix colour mess
      dev.off()
      
      ## TIME
      id <- "T"
      dataNames=strConv(id)
      pdf(file = paste(outdir, "/", test, "_", id, ".pdf", sep = ""))
      barplot(as.matrix(cbind(data[,dataNames],NA)), main = "netperf test", names.arg = c(protosNames,""), ylab = "Time [s]", legend.text = roundsText, args.legend = list("topleft",bty = "n",cex=0.8), beside = TRUE) ## todo fix ylim shortness
      trash <- dev.off()
    }
  }
}

if ("top" %in% testsNames) {
  test <- "top"
  dataFile <- paste(indir, "/", test, dataext, sep = "")
  if ( !file.exists(dataFile) ) { warning(msg=paste("Not plotting. File does not exist: ", dataFile, sep = ""))
  } else {
    ## > colnames(top)
    ##  [1] "usedK"        "freeK"        "shrdK"        "buffK"        "cachedK"     
    ##  [6] "usr100"       "sys100"       "nic100"       "idle100"      "io100"       
    ## [11] "irq100"       "sirq100"      "olsr1VSZ"     "olsr1100VSZ"  "olsr1100CPU" 
    ## [16] "bmxVSZ"       "bmx100VSZ"    "bmx100CPU"    etc
    data <- read.table(dataFile, header = TRUE)
    
    if (nrow(data)==0) { warning(msg=paste("Not plotting. Empty file: ", dataFile, sep = ""))
    } else {
      ## MEMORY
      id <- "VSZ"
      dataNames=strConv(id)
      pdf(file = paste(outdir, "/", test, "_", id, ".pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      matplot(data[,dataNames], main = "Memory usage", type = "l", lty = 1, xlab="Time [s]", ylab = "Memory [KB]", col = protoColours)
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('top', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      dev.off()
      
      ## CPU
      id <- "100CPU"
      dataNames=strConv(id)
      pdf(file = paste(outdir, "/", test, "_", id, ".pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      matplot(data[,dataNames], main = "CPU load", type="l", lty = 1, xlab="Time [s]", ylab = "CPU [%]", col=protoColours)
      for (i in 1:length(dataNames)) {lines(par('usr')[1:2], rep(mean(data[,dataNames[i]],na.rm = TRUE),2), type="l", lty = 2, col=protoColours[i])}
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('top', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      trash <- dev.off()
    }
  }
}

if ("tcpdump" %in% testsNames) {
  test <- "tcpdump"
  dataFile <- paste(indir, "/", test, dataext, sep = "")
  if ( !file.exists(dataFile) ) { warning(msg=paste("Not plotting. File does not exist: ", dataFile, sep = ""))
  } else {
    ##  [1] "time"    "totalP"  "totalB"  "olsr1P"  "olsr1B"  "bmxP"    "bmxB"   etc
    data <- read.table(dataFile, header = TRUE)
    
    if (nrow(data)==0) { warning(msg=paste("Not plotting. Empty file: ", dataFile, sep = ""))
    } else {
      ## OVERHEAD PACKETS
      id <- "P"
      dataNames=strConv(id)
      pdf(file = paste(outdir, "/", test, "_", id, ".pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      matplot(data[,dataNames], main = "Network overhead", type="l", lty = 1, xlab="Time [s]", ylab = "Overhead [Packets/s]", col=protoColours)
      for (i in 1:length(dataNames)) { lines(par('usr')[1:2], rep(mean(data[,dataNames[i]],na.rm = TRUE),2), type="l", lty = 2, col=protoColours[i]) }
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('top', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      dev.off()
      
      ## OVERHEAD BANDWIDTH
      id <- "B"
      dataNames=strConv(id)
      pdf(file = paste(outdir, "/", test, "_", id, ".pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      matplot(data[,dataNames], main = "Network overhead", type="l", lty = 1, xlab="Time [s]", ylab = "Overhead [B/s]", col=protoColours)
      for (i in 1:length(protosNames)) { lines(par('usr')[1:2], rep(mean(data[,dataNames[i]],na.rm = TRUE),2), type="l", lty = 2, col=protoColours[i]) }
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('center', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      trash <- dev.off()
    }
  }
}

quit(save="no")
