library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 2) {
  stop("Usage analyze_maximum_thoughput.r <input.csv> <outputprefix>")
}

inFile <- args[1] 
outFile <- args[2]

startPng <- function(file) {
    png(paste(file, ".png", sep=""), height=450, width=900, bg="white")
    base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
    par(bg=base2)
}

startPng(outFile)

res = read.csv(inFile, header=TRUE)
res$stdTps = as.numeric(as.character(res$stdTps))
plot(
      res$clientConcurrencies
    , res$throughput
    , type="n" # Don't plot yet
    , xlab="Total number of virtual threads (no unit)"
    , ylab="Average total throughput (transactions/second)"
    , xlim=c(min(res$conc), max(res$conc))
    , ylim=c(min(res$avgTps-res$stdTps), max(res$avgTps+res$stdTps))
    )

threads = unique(res$threads)
for (i in threads) {
  dat = res[res$threads == i,]
  conc = dat$conc
  avg = dat$avgTps
  sdev = dat$stdTps
  lines(conc, avg)
  arrows(conc, avg-sdev, conc, avg+sdev, length=0.05, angle=90, code=3)
}
