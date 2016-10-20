library(caTools)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage analyze_stability_trace.r <resultsfile> <plotprefix> <outputdir>")
}

resFile <- args[1] 
filePrefix <- args[2]
outDir <- args[3]
outFileTps <- paste(filePrefix, "tps", sep="-")
outFileAvg <- paste(filePrefix, "resp", sep="-")

res = read.csv(resFile, header=TRUE)

startPng <- function(file) {
  png(filename=paste(paste(outDir, file, sep="/"), "png", sep="."), height=450, width=900, bg="white")
  base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
  par(bg=base2)
}

nrClients = max(res$client) # Starting rom zero

startPng(outFileTps)
plot(
    res$second
  , res$tps
  , main="Throughput"
  , xlab="Time elapsed since start of experiment (seconds)"
  , ylab="Throughput (transactions/second)"
  , pch=4 # Point shape: Cross
  , col=adjustcolor("black", alpha.f=(1/3))
  )

startPng(outFileAvg)
plot(
    res$second
  , res$avg/1000
  , main="Response time"
  , xlab="Time elapsed since start of experiment (seconds)"
  , ylab="Response time (milliseconds)"
  , xlim=c(0,3600)
  , ylim=c(150,500)
  , pch=4
  , col=adjustcolor("black", alpha.f=(1/2))
  )
arrows(
    res$second
  , (res$avg-res$std)/1000
  , res$second
  , (res$avg+res$std)/1000
  , length=0 # Length of arrow head
  , lwd=5 # Line width
  , col=adjustcolor("black", alpha.f=(1/20))
  )

