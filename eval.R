#library(Hmisc)


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

ecdfData <- function(D, what, asLog, sameLen) {
  
  scatterPlot=FALSE
  yMax <- 0.8
  
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
    
    legendProt <- c(legendProt, paste(prot, " n=",lenProt[[prot]],"/",length(probes), sep=""))
    legendSym  <- c(legendSym,  baseSymbol)    
    baseSymbol=baseSymbol+1
  }
  
  
  par(mar=c(5,5,2,5), xlog=TRUE)
  plot.ecdf( valMax*2, xlim=c(valMin,valMax), ylim=(c(0,yMax)), verticals=FALSE, do.points=FALSE, 
             xlab=what, ylab="ECDF", main="", 
             log=asLog,
             add=FALSE )

  grid(col="grey")
  axis(4)
  
  qy=c(0.5)
  qy=seq(0,yMax,0.1)
  abline(h=qy, col="grey")
  
#  qx=c()
#  abline(v=qx, col="grey")

  if (scatterPlot==TRUE) {
    baseSymbol=1
    for (prot in allProtocols) {
      points( (D[["RTT"]][D$PROTO==prot]), 
              (D[["HOPS"]][D$PROTO==prot] + ((baseSymbol-2)/10))/10,
              pch=3,#baseSymbol,
              cex=0.5,
              col=baseSymbol )
      
      baseSymbol=baseSymbol+1
    }
    mtext("HOPS/10",side=4,line=3)
    legend(x=valMin,y=(yMax-0.2), legendProt, pch=3, col=legendSym, title="HOPS", cex=0.8)
  }  
  
  baseSymbol=1
  for (prot in allProtocols) {
    V <- allVals[allGroups==prot]
    E <- ecdf(V)
    plot(E, col=baseSymbol, lty=1, cex.lab=1, cex.axis=1, lwd=2,
         verticals=TRUE, do.points=TRUE, pch=3, cex=0.5, add=TRUE)
    
    qp <- array(c(quantile(E,probs=qy),qy), dim=c(length(qy),2))
#   points(qp, pch=baseSymbol, col=baseSymbol)
#   abline(v=qp[,1], col=baseSymbol)
    
    baseSymbol=baseSymbol+1
  }

  
# legend("topleft",     legendProt, pch=legendSym,col=legendSym)
  legend(x=valMin,y=yMax, legendProt, lty=1, col=legendSym, title="ECDF", cex=0.8)
  
#  Ecdf( allVals, 
#        xlab=what, 
#        group=allGroups, 
#        label.curves=list(keys=1:length(allProtocols)),
#        q=c(0.5),
#        xlim=c(valMin-1,valMax/100))
}


D <- read.table("tmp.data", header=TRUE)

S <- D$GRP==0
#S <- S | D$GRP==1 
#S <- S | D$GRP==2
S <- S | D$GRP==3

D <- data.frame( PROTO=D[["PROTO"]][S] , RTT=D[["RTT"]][S], TTL=D[["TTL"]][S] )

D$HOPS <- ( max( D$TTL[!is.na(D$TTL)] ) +1 ) - D$TTL
ecdfData(D,"RTT","x", TRUE)
#ecdfData(D,"HOPS","", TRUE)

#timeData(D)










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




