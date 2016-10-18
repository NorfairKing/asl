library(caTools)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage analyze_stability_trace.r <resultsfile> <plotprefix> <outputdir>")
}

resFile <- args[1] 
filePrefix <- args[2]
outDir <- args[3]
outFileTps <- paste(filePrefix, "tps", sep="-")
outFileAvg <- paste(filePrefix, "avg", sep="-")

res = read.csv(resFile, header=TRUE)

startPng <- function(file) {
  png(filename=paste(paste(outDir, file, sep="/"), "png", sep="."), height=600, width=800, bg="white")
  base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
  par(bg=base2)
}

for (kind in c("read", "write")) {
  ofkinds = res[res$Kind == kind,]
  firstInstant = min(ofkinds$ParsedTime)
  reltime = ofkinds$ReceivedTime - firstInstant
  seclen = 1000 * 1000 * 1000
  reltimesecs = reltime / seclen
  reltimemins = reltimesecs / 60

  responsetime = ofkinds$RespondedTime - ofkinds$ReceivedTime
  responsetime = responsetime / 1000

  startPng(paste(filePrefix, kind, "resp", sep="-"))
  plot(
      reltimemins
      , responsetime
      , main=paste("Response time (", kind, ")", sep="")
      , xlab="Relative time since start (minutes)"
      , ylab="Response time (ms)"
      , log="y")

  nrBucks = floor(max(reltimemins)-1)
  respComb = matrix(0, nrow=nrBucks, ncol=5)
  throughput = matrix(0, nrow=nrBucks, ncol=1)
  for (i in 0:nrBucks) {
    bucket = ofkinds[floor(reltimemins) == i,]
    resptimebuck = bucket$RespondedTime - bucket$ReceivedTime
    resptimebuck = resptimebuck / 1000
    respComb[i, 1] = quantile(resptimebuck, probs = 0.05, names=FALSE, na.rm=TRUE)
    respComb[i, 2] = quantile(resptimebuck, probs = 0.25, names=FALSE, na.rm=TRUE)
    respComb[i, 3] = quantile(resptimebuck, probs = 0.5 , names=FALSE, na.rm=TRUE)
    respComb[i, 4] = quantile(resptimebuck, probs = 0.75, names=FALSE, na.rm=TRUE)
    respComb[i, 5] = quantile(resptimebuck, probs = 0.9 , names=FALSE, na.rm=TRUE)
    throughput[i, 1] = length(resptimebuck / 60)
  }
  x = 1:nrBucks
  lines(x,respComb[,1], lwd=2, col="red")
  lines(x,respComb[,2], lwd=2, col="red")
  lines(x,respComb[,3], lwd=2, col="red")
  lines(x,respComb[,4], lwd=2, col="red")
  lines(x,respComb[,5], lwd=2, col="red")
}
