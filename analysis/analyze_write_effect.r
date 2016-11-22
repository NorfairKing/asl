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
res$writePercentage <- as.numeric(as.character(res$writePercentage))
res$respstd <- as.numeric(as.character(res$respstd))
res$tpsstd <- as.numeric(as.character(res$tpsstd))
  
colors = c("blue", "red")

### THROUGHPUT ###
for (nrSers in unique(res$nrServers)) {
  file = paste(prefix, "tps", nrSers, sep="-")
  startPng(file)

	plot(
        res$writePercentage
      , res$tpsavg
      , type="n" # Don't plot yet
      , xlab="Write percentage (write requests / request)"
      , ylab="Average total throughput (transactions/second)"
      , ylim=c(min(res$tpsavg-res$tpsstd), max(res$tpsavg+res$tpsstd))
      )
  
  predat <- res[res$nrServers == nrSers, ]

  i = 1
  for (rFac in unique(predat$replicationFactor)) {
    dat <- predat[predat$replicationFactor == rFac,]

    avgTps <- dat$tpsavg
    stdTps <- dat$tpsstd
    wper <- dat$writePercentage

    lines(wper, avgTps, col=colors[i])
    arrows(wper, avgTps-stdTps, wper, avgTps+stdTps, length=0.05, angle=90, code=3, col=colors[i])
    i = i + 1
  }

  my.legend.size <- legend("topright"
      , c("R=1", "R=all")
      , lty=c(1,1) # gives the legend appropriate symbols (lines)
      , lwd=c(2.5,2.5)
      , col=colors) # gives the legend lines the correct color and width
  title(paste("Througput,", nrSers, "servers"))
}

### Response time ###
for (nrSers in unique(res$nrServers)) {
  file = paste(prefix, "resp", nrSers, sep="-")
  startPng(file)

	plot(
        res$writePercentage
      , res$respavg
      , type="n" # Don't plot yet
      , xlab="Write percentage (write requests / request)"
      , ylab="Average response time (microseconds)"
      , ylim=c(min(res$respavg-res$respstd), max(res$respavg+res$respstd))
      )
  
  predat <- res[res$nrServers == nrSers, ]

  i = 1
  for (rFac in unique(predat$replicationFactor)) {
    dat <- predat[predat$replicationFactor == rFac,]

    avgResp <- dat$respavg
    stdResp <- dat$respstd
    wper <- dat$writePercentage

    lines(wper, avgResp, col=colors[i])
    arrows(wper, avgResp-stdResp, wper, avgResp+stdResp, length=0.05, angle=90, code=3, col=colors[i])
    i = i + 1
  }

  my.legend.size <- legend("topright"
      , c("R=1", "R=all")
      , lty=c(1,1) # gives the legend appropriate symbols (lines)
      , lwd=c(2.5,2.5)
      , col=colors) # gives the legend lines the correct color and width
  title(paste("Response time,", nrSers, "servers"))
}
