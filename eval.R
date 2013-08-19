#library(Hmisc)
library(plotrix)


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
    legendProt <- c(legendProt, paste(prot, " n=",lenProt[[prot]], sep=""))
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
  mtext("HOPS",side=4,line=3)
  legend("topright", title="HOPS", legendProt, lty=1, col=legendSym, cex=0.8)
  legend("topleft",  title="RTT",  legendProt, pch=3, col=legendSym, cex=0.8)
  #  legend("topleft",col=c("red","blue"),lty=1,pch=3,legend=c("y1","y2"))  
}



ecdfData <- function(D, what, xLabel, asLog, sameLen, scatterPlot, statsTable, yMax, qx) {
  
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
    
    legendProt <- c(legendProt, paste(prot, " n=",sum(!is.na(probes)),"/", length(probes), sep=""))
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

groups <- c(
  1,     # mobile_node0 
  2,     # mobile_node1 
#  3,     # mobile_running_test0
#  4:21,  # random_ping_test0
#  22:28, # random_ping_test1,
  "INVALID"
  )

D <- getSubset( (read.table("tmp.data", header=TRUE)), "GRP", groups)
S <- getSubset( (read.table("tmp.stat", header=TRUE)), "GRP", groups)

ecdfData(D,"RTT", "round tip time (RTT) [ms]","x", FALSE, FALSE,TRUE, 1, c(10,100,1000))
#ecdfData(D,"HOPS", "number of hops","", FALSE, FALSE,FALSE, 1, c(max(D[["HOPS"]],na.rm=TRUE)))

#timeData(D)

#TODO:
# RTT over HOPS
# Hops histogram
# quantiles cases
# title
# legends









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




