library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage analyze_maximum_thoughput.r <common.r> <input.csv> <outputprefix>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]

res = read.csv(inFile, header=TRUE)
res$stdTps = as.numeric(as.character(res$stdTps))

threads = unique(res$threads)
for (i in threads) {
  file = paste(outFile, i, sep="-")
  startPng(file)

  dat = res[res$threads == i,]
  conc = dat$conc
  avg = dat$avgTps
  sdev = dat$stdTps

  plot(
        dat$clientConcurrencies
      , dat$throughput
      , type="n" # Don't plot yet
      , xlab="Total number of virtual threads (no unit)"
      , ylab="Average total throughput (transactions/second)"
      , xlim=c(min(res$conc), max(res$conc))
      , ylim=c(min(res$avgTps-res$stdTps), max(res$avgTps+res$stdTps))
      )
  lines(conc, avg)
  arrows(conc, avg-sdev, conc, avg+sdev, length=0.05, angle=90, code=3)
  abline(h = max(res$avgTps), col="red")
  my.legend.size <- legend("bottomright"
      , "Maximum"
      , lty=c(1)
      , lwd=c(2.5)
      , col="red")
}
