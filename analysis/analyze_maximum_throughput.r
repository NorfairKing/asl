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
plot(res$clientConcurrencies, res$throughput)

threads = unique(res$middleThreads)
for (i in threads) {
  dat = res[res$middleThreads == i,]
  lines(dat$clientConcurrencies, dat$throughput)
}
