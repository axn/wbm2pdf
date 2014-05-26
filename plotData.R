#!/usr/bin/Rscript

knownTests <- c("netperf", "top", "tcpdump") ## used to define testsNames
knownProtocols <- c("olsr1", "bmx", "batadv", "babel", "olsr2") ## used to define protocolNames
protoColours <- c( "black", "red", "green", "blue", "darkgoldenrod") ## overwritten if param passed
indir <- "." ## overwritten if param passed
outdir <- "." ## overwritten if param passed
decorate <- TRUE

args <- commandArgs(trailingOnly = TRUE)

helpText <- function(){
  cat('Syntax 
  ./$0 [--args arguments]
    do not forget the heading "--args" to pass arguments
    lists are of the form "element1 element2 ..."
Arguments:
  --tests=list - tests to plot; default: all; available:',knownTests,'all
  --protos=list - protocols to plot; default: all; available:',knownProtocols,'all
  --colours=list - protocol colours (pairs with --proto); default:',protoColours,'
  --indir=path/to/dir - input directori, where *.data files are
  --outdir=path/to/dir - output dir, where *.pdf are saved
  --decorate=TRUE|FALSE - add legends; default: TRUE (TODO: extend to other fields? or remove opt?)
  --help   - print this text
Example:
  ./$0 [--args --tests="netperf top" --protos="olsr1 bmx"] \n')
}

if("--help" %in% args) { helpText(); q(save = "no") }

## Parse arguments (we expect the form --arg=value)
parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))
names(argsL) <- argsDF$V1

#### default values
## plots
if(is.null(argsL$tests)) { testsNames <- "all"
} else {
  testsNames <- strsplit(argsL$tests, " ")[[1]] ## [[1]] to extract the first element of the list, which is a vector
}
if(FALSE %in% { testsNames %in% c(knownTests, "all") }){ print("Error: Unknow test."); helpText(); quit(save = "no") }
if("all" %in% testsNames) { testsNames <- knownTests }

## protos
if(is.null(argsL$protos)) { protosNames <- "all"
} else {
  protosNames <- strsplit(argsL$proto, " ")[[1]]
}
if(FALSE %in% { protosNames %in% c(knownProtocols, "all") }){ print("Warning: Unknow protocol."); helpText() }
if("all" %in% protosNames) { protosNames <- knownProtocols }

## colours
if(!is.null(argsL$colours)) { protoColours <- replace(protoColours, 1:length(strsplit(argsL$colours, " ")[[1]]), strsplit(argsL$colours, " ")[[1]]) }
if(FALSE %in% { protoColours %in% colours() }){ print("Error: Unknow colour. See colours() in R."); quit(save = "no") }

if(!is.null(argsL$indir)) { indir <- argsL$indir}
if(!is.null(argsL$outdir)) { outdir <- argsL$outdir}

if(!is.null(argsL$decorate)) { decorate <- argsL$decorate}


#### PLOTS

if("netperf" %in% testsNames) {
  dataFile <- paste(indir, "/netperf.data", sep = "")
  if ( !file.exists(dataFile) ) { print(paste("[R] Warning: File does not exist. ", dataFile, sep = ""))
  } else {
    netperf <- read.table(dataFile, header = TRUE)
    ##  [1] "olsr1TP"  "olsr1T"   "bmxTP"    "bmxT"     etc
    
    if (nrow(netperf)==0) { print(paste("[R] Warning: Not plotting due to empty file. ", dataFile, sep = ""))
    } else {
      ## roundsText<-c("round1","round2",...)
      roundsText = NULL; for (i in 1:length(netperf[,1])) {roundsText <- c(roundsText, paste("round", i, sep=""))}
      
      ## THROUGHPUT
      pdf(file = paste(outdir, "/netperf_TP.pdf",sep = ""))
      dataNames = NULL; for(i in protosNames){dataNames <- c(dataNames, paste(i, "TP", sep=""))} ## TODO create a function("colID")
      
      ## empty column to make room for legend; as a consequence xlab must also be faked
      barplot(as.matrix(cbind(netperf[,dataNames],NA)), main = "netperf test", names.arg = c(protosNames,""), ylab = "Throughput [KB/s]", legend.text = roundsText, args.legend = list("topleft",bty = "n",cex=0.8), beside = TRUE) ## todo fix ylim shortness
      
      for(i in 1:length(dataNames)){lines(par('usr')[1:2], rep(mean(netperf[,dataNames[i]],na.rm = T),2), type="l", lty = 2, col=protoColours[i])} ## TODO fix colour mess
      dev.off()
      
      ## TIME
      pdf(file = paste(outdir, "/netperf_T.pdf", sep = ""))
      dataNames = NULL; for(i in protosNames){dataNames <- c(dataNames, paste(i, "T", sep=""))} ## TODO create a function("colID")
      barplot(as.matrix(cbind(netperf[,dataNames],NA)), main = "netperf test", names.arg = c(protosNames,""), ylab = "Time [s]", legend.text = roundsText, args.legend = list("topleft",bty = "n",cex=0.8), beside = TRUE) ## todo fix ylim shortness
      trash <- dev.off()
    }
  }
}

if ("top" %in% testsNames) {
  dataFile <- paste(indir, "/top.data", sep = "")
  if ( !file.exists(dataFile) ) { print(paste("[R] Warning: File does not exist. ", dataFile, sep = ""))
  } else {
    top <- read.table(dataFile, header = TRUE)
    ## > colnames(top)
    ##  [1] "usedK"        "freeK"        "shrdK"        "buffK"        "cachedK"     
    ##  [6] "usr100"       "sys100"       "nic100"       "idle100"      "io100"       
    ## [11] "irq100"       "sirq100"      "olsr1VSZ"     "olsr1100VSZ"  "olsr1100CPU" 
    ## [16] "bmxVSZ"       "bmx100VSZ"    "bmx100CPU"    etc
    
    if (nrow(top)==0) { print(paste("[R] Warning: Not plotting due to empty file. ", dataFile, sep = ""))
    } else {
      ## MEMORY
      pdf(file = paste(outdir, "/top_memory.pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      dataNames=NULL; for(i in protosNames){dataNames <- c(dataNames, paste(i, "VSZ", sep=""))} ## TODO create a function("colID")
      matplot(top[,dataNames], main = "Memory usage", type = "l", lty = 1, xlab="Time [s]", ylab = "Memory [KB]", col = protoColours)
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('center', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      dev.off()
      
      ## CPU
      pdf(file = paste(outdir,"/top_cpu.pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      dataNames=NULL; for(i in protosNames){dataNames <- c(dataNames, paste(i, "100CPU", sep=""))} ## TODO create a function("colID")
      matplot(top[,dataNames], main = "CPU load", type="l", lty = 1, xlab="Time [s]", ylab = "CPU [%]", col=protoColours)
      for (i in 1:length(dataNames)) {lines(par('usr')[1:2], rep(mean(top[,dataNames[i]],na.rm = T),2), type="l", lty = 2, col=protoColours[i])}
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('center', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      trash <- dev.off()
    }
  }
}

if ("tcpdump" %in% testsNames) {
  dataFile <- paste(indir, "/tcpdump.data", sep = "")
  if ( !file.exists(dataFile) ) { print(paste("[R] Warning: File does not exist. ", dataFile, sep = ""))
  } else {
    tcpdump <- read.table(paste(indir, "/tcpdump.data", sep = ""), header = TRUE)
    ## > colnames(tcpdump)
    ##  [1] "time"    "totalP"  "totalB"  "olsr1P"  "olsr1B"  "bmxP"    "bmxB"   etc
    
    if (nrow(tcpdump)==0) { print(paste("[R] Warning: Not plotting due to empty file. ", dataFile, sep = ""))
    } else {
      ## OVERHEAD PACKETS
      pdf(file = paste(outdir, "/tcpdump_overhead_P.pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      dataNames=NULL; for(i in protosNames){dataNames <- c(dataNames, paste(i, "P", sep=""))} ## TODO create a function("colID")
      matplot(tcpdump[,dataNames], main = "Network overhead", type="l", lty = 1, xlab="Time [s]", ylab = "Overhead [Packets/s]", col=protoColours)
      for (i in 1:length(dataNames)) { lines(par('usr')[1:2], rep(mean(tcpdump[,dataNames[i]],na.rm = T),2), type="l", lty = 2, col=protoColours[i]) }
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('center', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      dev.off()
      
      ## OVERHEAD BANDWIDTH
      pdf(file = paste(outdir, "/tcpdump_overhead_B.pdf", sep = ""))
      layout(rbind(1,2), heights=c(20,1))
      dataNames=NULL; for(i in protosNames){dataNames <- c(dataNames, paste(i, "B", sep=""))} ## TODO create a function("colID")
      matplot(tcpdump[,dataNames], main = "Network overhead", type="l", lty = 1, xlab="Time [s]", ylab = "Overhead [B/s]", col=protoColours)
      for (i in 1:length(protosNames)) { lines(par('usr')[1:2], rep(mean(tcpdump[,dataNames[i]],na.rm = T),2), type="l", lty = 2, col=protoColours[i]) }
      par(mar=c(0, 0, 0, 0))
      plot.new()
      if (decorate) { legend('center', bty = "n", protosNames, pch = 3, col = protoColours, cex=0.8, horiz = TRUE) }
      trash <- dev.off()
    }
  }
}

quit(save="no")
