#!/usr/bin/Rscript

#library(Hmisc)
library(plotrix)
library(stringr)

ecdfYforX <- function(data, X) {

  ecdfyforx <- function(data, x) {
    length(data[ data<=x ]) / length(data)
  }
  
  results <- c()
# print(paste("X=",X))
  for (x in c(X)) {
#   print(paste("x=",x))
    results <- c( results, ecdfyforx(data, x) )
  }
  
  results
}

ecdfXforY <- function(data, y) {
  pf <- (y)*length(data)
  pd <- (pf %/% 1) + (ifelse((pf %% 1)>0, 1, 0))
  sort(data)[pd]
}

ecdfYElements <- function(data) {
  L <-(sapply(lapply(split(data,c(rep("G",length(data)))), ecdf), function(e) e(data)))
  V <- L[1:length(data)]
  as.double(levels(factor(V)))
}

ecdfXElements <- function(data) {
  ecdfXforY(data,ecdfYElements(data))
}

timeData <- function(D) {
  allProtocols <- levels(D$PROTO)

  rttMax <- 0
  hopMax <- 0
  lenMax <- 0
  rttProbes <- list()
  hopProbes <- list()
  lenProt   <- list()
  legendProt=c()
  legendSym=c()  
  
  for (prot in allProtocols) {
    rttProbes[[prot]] <- D[["RTT"]][D$PROTO==prot]
    hopProbes[[prot]] <- D[["HOPS"]][D$PROTO==prot]
    
    hopMax <- max(c(hopMax,hopProbes[[prot]][!is.na(hopProbes[[prot]])]))
    rttMax <- max(c(rttMax,rttProbes[[prot]][!is.na(rttProbes[[prot]])]))
    lenMax <- max(c(lenMax,length(rttProbes[[prot]])))
    lenProt[[prot]] <- length(rttProbes[[prot]])
  }

  baseSymbol=1
  for (prot in allProtocols) {
    length(rttProbes[[prot]]) <- lenMax
    length(hopProbes[[prot]]) <- lenMax
    legendProt <- c(legendProt, 
        paste(prot, " n=",
          sum(S[["maxSeq"]][S$PROTO==prot]),"/",
          lenProt[[prot]],"/",
          sum(S[["SEQMAX"]][S$PROTO==prot]), sep=""))
    legendSym  <- c(legendSym,  baseSymbol)
    baseSymbol=baseSymbol+1
  }
  
  par(mar=c(5,4,4,5)+.1, ylog=TRUE)
  plot(rep(rttMax,lenMax), type="n", log="y", ylim=c(1,rttMax), ylab="")
  baseSymbol=1
  for (prot in allProtocols) {
    points(rttProbes[[prot]], type="p",pch=3,cex=0.2,col=baseSymbol)
    baseSymbol=baseSymbol+1
  }

  
  par(new=TRUE)
  plot(rep(hopMax,lenMax), type="n", ylim=c(1,hopMax), axes=F, ylab="")
  baseSymbol=1
  for (prot in allProtocols) {
    lines((hopProbes[[prot]])+(baseSymbol/20), type="l",lwd=3,col=baseSymbol)
    baseSymbol=baseSymbol+1
  }
  
  
  
  
  axis(4)
  mtext("RTT",side=2,line=3)
  legend("topleft",  title="RTT",  legendProt, pch=3, col=legendSym, cex=0.8)
  mtext("HOPS",side=4,line=3)
  legend("topright", title="HOPS", allProtocols, lty=1, col=legendSym, cex=0.8)
  #  legend("topleft",col=c("red","blue"),lty=1,pch=3,legend=c("y1","y2"))  
}



ecdfData <- function(S,D, what, xLabel, asLog, sameLen, scatterPlot, statsTable, yMax, qx) {
  
#  scatterPlot=FALSE
#  statsTable
#  yMax <- 1.5 #0.8
  qy=c(0.5)
# qx=c(10,100,1000)
  
  valMax <- 0
  valMin <- Inf
  lenMax <- 0
  lenMin <- Inf
  allRealMax <- 100000
  allVals    <- c()
  allGroups  <- c()
  
  allProtocols <- levels(D$PROTO)
  
  lenProt <- list()
  legendProt=c()
  legendSym=c()  
  
# par(mar=c(5,5,2,1), xlog=TRUE)

  
  for (prot in allProtocols) {
    probes <- D[[what]][D$PROTO==prot]
    valMax <- max(c(valMax,probes[!is.na(probes)]))
    valMin <- min(c(valMin,probes[!is.na(probes)]))
    lenMax <- max(c(lenMax,length(probes)))
    lenMin <- min(c(lenMin,length(probes)))
    lenProt[[prot]] <- length(probes)
  }
  
  baseSymbol=1
  for (prot in allProtocols) {
    probes         <- D[[what]][D$PROTO==prot]
    
    if (sameLen) 
      length(probes) <- lenMax
    
    numsTotal      <- length(probes)
    probesAllValid <- ifelse(is.na(probes), allRealMax, probes )
  
    allVals        <- c(allVals,   probesAllValid)
    allGroups      <- c(allGroups, rep(prot, numsTotal))
    
    legendProt <- c(legendProt, 
                    paste(prot, " n=",
                          sum(S[["maxSeq"]][S$PROTO==prot]),"/",
                          length(probes),"/",
                          sum(S[["SEQMAX"]][S$PROTO==prot]), sep=""))
    legendSym  <- c(legendSym,  baseSymbol)    
    baseSymbol=baseSymbol+1
  }
  
  
  par(mar=c(5,5,2,5), xlog=TRUE)
  plot.ecdf( valMax*2, xlim=c(valMin,valMax), ylim=(c(0,yMax)), verticals=FALSE, do.points=FALSE, 
             xlab=xLabel, ylab="ECDF", main="", 
             log=asLog,
             add=FALSE )

  grid(col="grey")
  axis(4)
  
  abline(h=qy, col="darkgrey")
  abline(v=qx, col="darkgrey")
  
  abline(h=seq(0,yMax,0.1), col="grey")
  
#  qx=c()
#  abline(v=qx, col="grey")

  stat <- array(dim=c((length(allProtocols)+1),(length(qx)+1)))
# stat[,1] <- c(NA,allProtocols)
  stat[1,] <- c(NA,paste(" ",qx))
  
  
  baseSymbol=1
  for (prot in allProtocols) {
    V <- allVals[allGroups==prot]
    E <- ecdf(V)
    plot(E, col=baseSymbol, lty=1, cex.lab=1, cex.axis=1, lwd=2,
         verticals=TRUE, do.points=TRUE, pch=3, cex=0.4, add=TRUE)
    
    qp <- array(c(quantile(E,probs=qy),qy), dim=c(length(qy),2))
    #   points(qp, pch=baseSymbol, col=baseSymbol)
    #   abline(v=qp[,1], col=baseSymbol)
    
    
#   points(ecdfXforY(V,qy), qy, col=baseSymbol, pch=baseSymbol, lwd=2, cex=1.5)
    qxy <- ecdfYforX(V,qx)
    points(qx, qxy, col=baseSymbol, pch=baseSymbol, lwd=2, cex=1.5)
#   text(qx,qxy,paste(qxy,"%"),adj=c(0,0),cex=0.8)
 
    stat[baseSymbol+1,] <- c(prot, paste(" ",as.integer(qxy*100)))
    baseSymbol=baseSymbol+1
  }
  legend(x=valMin,y=yMax, legendProt, lty=1, col=legendSym, pch=legendSym, bty="n", cex=0.8)
# legend("bottom", legendProt, lty=1, col=legendSym, title="ECDF", cex=0.8)
  
  if (statsTable==TRUE) {
    addtable2plot(valMin,yMax/(1/0.6), stat, display.colnames=FALSE, bty=FALSE, cex=0.8, 
                  hlines=TRUE, vlines=TRUE,
                  title=paste("proportion [%] <= ",what))  
  }
  
  if (scatterPlot==TRUE) {
#    par(new=TRUE)
    baseSymbol=1
    for (prot in allProtocols) {
      points( (D[["HOPS"]][D$PROTO==prot] + ((baseSymbol-2)/10)),
              (D[["RTT"]][D$PROTO==prot]) / 10,
              pch=3,#baseSymbol,
              cex=0.5,
              col=baseSymbol )
      
      baseSymbol=baseSymbol+1
    }
    mtext("RTT [10ms]",side=4,line=3)
    legend("bottomright", legendProt, pch=3, col=legendSym, title="RTT", cex=0.8)
  }  
  

  
  
#  Ecdf( allVals, 
#        xlab=what, 
#        group=allGroups, 
#        label.curves=list(keys=1:length(allProtocols)),
#        q=c(0.5),
#        xlim=c(valMin-1,valMax/100))
}


getSubset <- function(S, group, values) {
  s <- S[[group]]=="ILLEGAL"
  
  for (v in values) {
    s <- s | S[[group]]==v
  }
  
  subset(S,s)
}

error <- function(msgs="") {
  print(paste("ERROR: ", paste(msgs, sep=" ",collapse="! ")))
  terminate()
}

getArgList <- function( validArgs ) {

  args       <- commandArgs(TRUE)
  argList    <- list()
  argNum     <- 0
  argPattern <- "^--.*="
  argNew     <- validArgs[1]
  
  for (a in args) {
    
    argFound <- str_extract( a, argPattern)
    if ( class(argFound)=="character" ) {
      argName <- sub( sub( argFound, pattern="^--", replacement="" ), pattern="=$", replacement="" )
      argVals <- sub( a, pattern=argFound, replacement="" )
      
      if (class(argName)=="character" && class(argVals)=="character") {
        
        if (argName==validArgs[1]) {
          argNum <- argNum + 1
        } else if ( sum(validArgs==argName) == 0 ) {
          error("Invalid arg: ", argName)
        }
        
        if (argNum >= 1) {
          argList[[argName]][argNum] <- argVals
        } else {
          error("First argument must be: ", argNew)
        }
        
        for (i in 1:length(argList)) 
          length(argList[[i]]) <- argNum
      
      } else {
        error( c("Invalid arg structure: ",a))
      }
    } else {
      error( c("arg: ",a) )
    }
    
  }
  print(argList)
  argList
}




print("hello")
print(args)
argList <- getArgList(c("plot","type","groups"))

D <- read.table("tmp.data", header=TRUE)
S <- read.table("tmp.stat", header=TRUE)

if (length(argList$plot)>=1) {
  for (i in 1:length(argList$plot)) {
    
  
    groups <- c(
      #  1,     # mobile_node0 
      #  2,     # mobile_node1 
      #  3,     # mobile_running_test0
      #  4:21,  # random_ping_test0
      #  22:28, # random_ping_test1,
      "INVALID"
    )
    
    if (length(argList$groups) >= i) {
      for (g in unlist(strsplit(argList$groups[i],","))) {
        groups <- c( groups, as.integer(g))
      }
    }
    
    d <- getSubset( D, "GRP", groups)
    s <- getSubset( S, "GRP", groups)
    
    if (length(argList$type) >= i) {
      
      if (length(argList$plot) >= i)
        pdf( file=argList$plot[i] )
      
      
      if (argList$type[i]=="ecdfVsRtt") {
        ecdfData(s,d,"RTT", "round tip time (RTT) [ms]","x", FALSE, FALSE,TRUE, 1, c(10,100,1000))
      } else if (argList$type[i]=="ecdfVsHops") {
        ecdfData(s,d,"HOPS", "number of hops","", FALSE, FALSE,FALSE, 1, c(max(d[["HOPS"]],na.rm=TRUE)))
      } else if (argList$type[i]=="rttVsHops") {
        rttVsHops(s,d)
      } else if (argList$type[i]=="dataVsTime") {
        timeData(d)
      } else {
        error("Unkown type: ", argList$type[i])
      }
      
      if (length(argList$plot) >= i)
        dev.off()
    }  
  }
}
#TODOs:
# RTT over HOPS
# Hops histogram
# boxplots
# title


#q(status=0)





#######################################################
#######################################################
#######################################################
#######################################################




ttlHist <-function(d){
  ttlMax <- 64
  hops   <- 1 - (d - ttlMax)
  hist(hops, seq(0, max(hops), 1))
}

rttHist <-function(d){
  rttMin <- 0
  rttMax <- max(d)
  hist(d, seq(rttMin, rttMax+1, 1))
}




