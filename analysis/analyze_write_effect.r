library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Usage analyze_write_effect.r <common.r> <input.csv>")
}

common <- args[1]
source(common)
inFile <- args[2]
prefix <- args[3]

res = read.csv(inFile, header=TRUE)

# xl = "Replication factor"
# yl = "Response time (microseconds)"
# 
for (nrSers in unique(res$nrServers)) {
  file = paste(prefix, nrSers, sep="-")
  startPng(file)

  title = paste("Response time,", nrSers, "servers")
	plot(
        res$writePercentage
      , res$tpsavg
      , type="n" # Don't plot yet
      , xlab="Write percentage (write requests / request)"
      , ylab="Average total throughput (transactions/second)"
      , ylim=c(min(res$tpsavg-res$tpsstd), max(res$tpsavg+res$tpsstd))
      )
  
  predat <- res[res$nrServers == nrSers, ]

  for (rFac in unique(predat$replicationFactor)) {
    dat <- predat[res$replicationFactor == rFac,]

    avgTps <- dat$tpsavg
    stdTps <- dat$tpsstd
    wper <- dat$writePercentage

    lines(wper, avgTps)
    arrows(wper, avgTps-stdTps, wper, avgTps+stdTps, length=0.05, angle=90, code=3)
  }
}
