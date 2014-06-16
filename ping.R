#!/usr/bin/Rscript

#library(Hmisc)
library(plotrix)
library(stringr)


colors <- c(rgb(0,0,0,1/3),rgb(1,0,0,1/4),rgb(0,1,0,1/4),rgb(0,0,1,1/3))

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

printStats <- function(xlim, ylim, S,D, asLog="") {
  allProtocols <- levels(D$PROTO)
  xMin <- xlim[1]
  xMax <- xlim[2]
  yMin <- ylim[1]
  yMax <- ylim[2]
  
  content <- array(dim=c((length(allProtocols)+1),5))
  content[1,] <- c("","succ","used","expect","rcvd")
  
  p=1
  for (prot in allProtocols) {
    probes <- D[["RTT"]][D$PROTO==prot]
    content[1+p,] <- c(prot,
                       sum(!is.na(probes)),
                       sum(S[["maxSeq"]][S$PROTO==prot]),
                       length(probes),
                       sum(S[["SEQMAX"]][S$PROTO==prot])
    )
    p=p+1
  }
  
  groups <- as.integer(levels(factor(S[["GRP"]])))
  
  nodes <- c()
  for (n in S[["NODE"]]) {
    if (length(nodes)==0 || nodes[length(nodes)]!=n) {
      nodes <- c(nodes,n)
    }
  }
  
  if(str_detect(asLog,"x"))
    xPos <- 10^(log10(xMin)+((log10(xMax)-log10(xMin))/5))
  else
    xPos <- xMin+((xMax-xMin)/5)
  
  #addtable2plot(xPos, yMax, table=content, xjust=0, yjust=0, display.colnames=FALSE, bty=FALSE, cex=0.8, 
  #              hlines=TRUE, vlines=TRUE,
  #              title=( paste("groups=",paste(groups,sep="",collapse=","),
  #                            (if(length(nodes)>3) "" else paste(" to=",paste(nodes,sep="",collapse=","), sep="" ))
  #                            , sep="" ) ) )
}

rttVsHops <- function (S,D) {
  allProtocols <- levels(D$PROTO)
  xlabHopMin <- 5
  
  rttMax <- 0
  hopMax <- 0
  lenMax <- 0
  hstMax <- 0
  
  rttProbes <- list()
  hopProbes <- list()
  histProt   <- c()
  
  p=1
  for (prot in allProtocols) {
    rttProbes[[prot]] <- D[["RTT"]][D$PROTO==prot]
    hopProbes[[prot]] <- D[["HOPS"]][D$PROTO==prot]
        
    hopMax <- max(c(hopMax,hopProbes[[prot]][!is.na(hopProbes[[prot]])]))
    rttMax <- max(c(rttMax,rttProbes[[prot]][!is.na(rttProbes[[prot]])]))
    lenMax <- max(c(lenMax,length(hopProbes[[prot]])))
    hstMax <- max(c(hstMax,length(hopProbes[[prot]][!is.na(hopProbes[[prot]])])))    
    p=p+1
  }
  
  p=1
  for (prot in allProtocols) {
    histProt[[prot]] <- hist( hopProbes[[prot]]+((p-0.5-(length(allProtocols)/2))/10), seq(0,hopMax+1,0.1), plot=F )  
    p=p+1
  }
  
  par(mar=c(5,4,4,5)+.1, ylog=TRUE)
  xlim=c(0.7,max(c(xlabHopMin,hopMax))+0.3)
  ylim=c(1,rttMax)
  plot(rep(rttMax,hopMax), type="n", log="y", ylim=ylim, xlim=xlim, ylab="RTT [ms]", xlab="hops")
  p=1
  for (prot in allProtocols) {
    points(hopProbes[[prot]]+((p-0.5-(length(allProtocols)/2))/10), rttProbes[[prot]], 
           type="p",pch=3,cex=0.2,col=colors[p])
    p=p+1
  }
  
  legend("topleft",  xjust=0, yjust=0, title="RTT", bty="n",  allProtocols, pch=3, col=colors, cex=0.8)
  
  printStats(xlim, ylim, S, D)
    
  
  par(new=TRUE)
  p=1
  for (prot in allProtocols) {
    plot(histProt[[prot]], col=colors[p], ylim=c(0,hstMax), xlim=xlim, add=(p!=1), 
         main="", xlab="", ylab="", axes=F)
    p=p+1
  }
  axis(4)
  mtext("Occurance",side=4,line=3)
  legend("topright", title="Occurance", bty="n", allProtocols, fill=colors, cex=0.8)
  
}



timeData <- function(S, D) {
  allProtocols <- levels(D$PROTO)

  rttMax <- 0
  hopMax <- 0
  lenMax <- 0
  rttProbes <- list()
  hopProbes <- list()
  
  for (prot in allProtocols) {
    rttProbes[[prot]] <- D[["RTT"]][D$PROTO==prot]
    hopProbes[[prot]] <- D[["HOPS"]][D$PROTO==prot]
    
    hopMax <- max(c(hopMax,hopProbes[[prot]][!is.na(hopProbes[[prot]])]))
    rttMax <- max(c(rttMax,rttProbes[[prot]][!is.na(rttProbes[[prot]])]))
    lenMax <- max(c(lenMax,length(rttProbes[[prot]])))
  }

  xlim=c(1,lenMax)
  ylim=c(1,rttMax)
  
  par(mar=c(5,4,4,5)+.1, ylog=TRUE)
  plot(rep(rttMax,lenMax), type="n", log="y", xlim=xlim, ylim=ylim, ylab="", xlab="icmp sequence number")
  p=1
  for (prot in allProtocols) {
    points(rttProbes[[prot]], type="p",pch=3,cex=0.2,col=colors[p])
    p=p+1
  }

  mtext("RTT [ms]",side=2,line=3)
  legend("topleft",  title="RTT", bty="n", allProtocols, pch=3, col=colors, cex=0.8)
  printStats(xlim, ylim, S, D)
  
  par(new=TRUE)
  plot(rep(hopMax,lenMax), type="n", ylim=c(1,max(6,hopMax)+0.5), axes=F, ylab="", xlab="")
  p=1
  for (prot in allProtocols) {
    lines((hopProbes[[prot]])+(p/20), type="l",lwd=3,col=colors[p])
    p=p+1
  }

  axis(4)  
  mtext("HOPS",side=4,line=3)
  legend("topright", title="HOPS", bty="n", allProtocols, lty=1, col=colors, cex=0.8)
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
  
  p=1
  for (prot in allProtocols) {
    probes         <- D[[what]][D$PROTO==prot]
    
    if (sameLen) 
      length(probes) <- lenMax
    
    numsTotal      <- length(probes)
    probesAllValid <- ifelse(is.na(probes), allRealMax, probes )
  
    allVals        <- c(allVals,   probesAllValid)
    allGroups      <- c(allGroups, rep(prot, numsTotal))
    
    p=p+1
  }
  
  xlim=c(valMin,valMax)
  ylim=(c(0,yMax))
  par(mar=c(5,5,2,5), xlog=TRUE)
  plot.ecdf( valMax*2, xlim=xlim, ylim=ylim, verticals=FALSE, do.points=FALSE,
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
  
  
  p=1
  for (prot in allProtocols) {
    V <- allVals[allGroups==prot]
    if (sum(!is.na(V))>=1) {
      E <- ecdf(V)
      plot(E, col=colors[p], lty=1, cex.lab=1, cex.axis=1, lwd=2,
           verticals=TRUE, do.points=TRUE, pch=3, cex=0.4, add=TRUE)
      
      qp <- array(c(quantile(E,probs=qy),qy), dim=c(length(qy),2))
      #   points(qp, pch=p, col=colors[p])
      #   abline(v=qp[,1], col=colors[p])
      
      
  #   points(ecdfXforY(V,qy), qy, col=colors[p], pch=p, lwd=2, cex=1.5)
      qxy <- ecdfYforX(V,qx)
      points(qx, qxy, col=colors[p], pch=p, lwd=2, cex=1.5)
  #   text(qx,qxy,paste(qxy,"%"),adj=c(0,0),cex=0.8)
   
      stat[p+1,] <- c(prot, paste(" ",as.integer(qxy*100)))
    }
    p=p+1
  }

  legend("topleft", allProtocols, lty=1, col=colors, pch=seq(1,length(allProtocols)), title="ECDF", bty="n", cex=0.8)
  printStats(xlim, ylim, S, D, asLog)
  
  
  if (statsTable==TRUE) {
    addtable2plot(valMin,yMax/(1/0.65), stat, display.colnames=FALSE, bty=FALSE, cex=0.8, 
                  hlines=TRUE, vlines=TRUE,
                  title=paste("proportion [%] <= ",what))  
  }
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

getArgList <- function( argStruct ) {

  args       <- commandArgs(TRUE)
  argList    <- list()
  argPattern <- "^--.*="
  argMax     <- 0
  
  for (s in 1:length(argStruct)) {
    validArgs  <- argStruct[[s]]
    argNum     <- 0
    argNew     <- validArgs[1]
    
    for (a in args) {
      
      argFound <- str_extract( a, argPattern)
      if ( class(argFound)=="character" ) {
        argName <- sub( sub( argFound, pattern="^--", replacement="" ), pattern="=$", replacement="" )
        argVals <- sub( a, pattern=argFound, replacement="" )
        
        if (class(argName)=="character" && class(argVals)=="character") {
          
          if ( sum(validArgs==argName) == 1 ) {
            
            if (argName==validArgs[1]) {
              argNum <- argNum + 1
              argMax <- max(argMax,argNum)
            }
            
            if (argNum >= 1) {
              argList[[argName]][argNum] <- argVals
            } else {
              error(c("First argument must be: ", argNew))
            }            
          }
        
        } else {
          error( c("Invalid arg structure: ",a))
        }
      } else {
        error( c("arg: ",a) )
      }
    }
    
    for (i in 1:length(validArgs)) {
      print( paste("arg=",validArgs[i], " <- ", argMax, sep="" ) )
      if (length(argList[[validArgs[i]]]) == 0)
        argList[[validArgs[i]]][argMax] <- NA
      else
        length(argList[[validArgs[i]]]) <- argMax
    }
    
  }
  print(argList)
  argList
}


setColors <- function ( D ) {

	  allProtocols <- levels(D$PROTO)
	  colors <- allProtocols

	  for (i in 1:length(allProtocols)) {
	      p = allProtocols[i]
	      if (p == "olsr1") {
	      	    colors[i] = "blue"
	      } else if (p == "bmx6") {
	      	    colors[i] = "green"
	      } else if (p == "batadv") {
	      	    colors[i] = "red"
	      } else if (p == "babel") {
	      	    colors[i] = "black"
	      } else if (p == "olsr2") {
	      	    colors[i] = "maroon1"
	      } else if (p == "cjdns") {
	      	    colors[i] = "darkgoldenrod"
	      } else {
	      	    colors[i] = "grey"
	      }
	  }
	  colors
}

createTexFigureSource <- function ( texFile, pdfFile, label=NA, descr=NA, width=NA, center=NA, figure=NA ) {
  
# label  <- if (!is.na(label) && class(label)=="character") label else ""
# descr  <- if (!is.na(descr) && class(descr)=="character") descr else ""
  width  <- if (is.na(width))  1 else as.single(width)
  center <- if (is.na(center)) 1 else as.integer(center)
  figure <- if (is.na(figure)) 1 else as.integer(figure)
  
  tex <- c(
    if (figure==1) "\\begin{figure}\n" else "",
    if (center==1) " \\centering\n" else "",
    " \\includegraphics[width=", as.character(width), "\\textwidth]{", pdfFile, "}\n",
    if (!is.na(descr)) paste(" \\caption{", descr, "}\n", sep="") else "",
    if (!is.na(label)) paste(" \\label{",   label, "}\n", sep="") else "",
    if (figure==1) "\\end{figure}\n" else ""
  )
  
  print( paste("writing tex image source to: ", texFile ) )
  cat(tex, file=texFile, sep="")
}


print("hello")
print(args)
argList <- getArgList(list(
                            images=c("imgdir"),
                            tex=c("texdir"),
                            input=c("data","stat"),
                            plots=c("name","type","groups","desc", "width", "center", "figure", "label")))

inDataFile <- if ( class(argList$data[1])=="character" ) argList$data[1] else "wbmv6.data"
inStatFile <- if ( class(argList$stat[1])=="character" ) argList$stat[1] else "wbmv6.stat"

D <- read.table( inDataFile, header=TRUE)
S <- read.table( inStatFile, header=TRUE)

colors <- setColors( D )

if (length(argList$name)>=1) {
  
  for (i in 1:length(argList$name)) {

    groups <- c(
      #  1,     # mobile_node0 
      #  2,     # mobile_node1 
      #  3,     # mobile_running_test0
      #  4:21,  # random_ping_test0
      #  22:28, # random_ping_test1,
      "INVALID"
    )
    
    if (!is.na(argList$groups[i])) {
      for (g in unlist(strsplit(argList$groups[i],","))) {
        groups <- c( groups, as.integer(g))
      }
    }
    
    d <- getSubset( D, "GRP", groups)
    s <- getSubset( S, "GRP", groups)
    
    if (!is.na(argList$type[i])) {
      
      pdfFile <- FALSE
      if (!is.na(argList$name[i]) >= i && !is.na(argList$imgdir[1])) {
        pdfFile=paste( argList$imgdir[1], "/", argList$name[i], ".pdf", sep="" )
        print( paste("writing pdf to: ", pdfFile ) )
        pdf( file=pdfFile )
      }
      
      if (pdfFile != FALSE && !is.na(argList$texdir[1])) {
        createTexFigureSource( paste(argList$texdir[1], "/", argList$name[i], ".tex", sep="" ), 
                              paste("", pdfFile, sep=""),
                              label=argList$name[i], 
                              descr=argList$desc[i],
                              width=argList$width[i],
                              center=argList$center[i],
                              figure=argList$figure[i])
      }
      
      
      if (argList$type[i]=="ecdfVsRtt") {
        ecdfData(s,d,"RTT", "round trip time (RTT) [ms]","x", FALSE, FALSE,FALSE, 1, c(10,100,1000))
      } else if (argList$type[i]=="ecdfVsHops") {
        ecdfData(s,d,"HOPS", "number of hops","", FALSE, FALSE,FALSE, 1, c(max(d[["HOPS"]],na.rm=TRUE)))
      } else if (argList$type[i]=="rttVsHops") {
        rttVsHops(s,d)
      } else if (argList$type[i]=="dataVsTime") {
        timeData(s,d)
      } else {
        error("Unkown type: ", argList$type[i])
      }
      
      if (pdfFile!=FALSE)
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




