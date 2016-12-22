args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage total_durs_histo.r <common.r> <input.csv> <output.png>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]

res = read.csv(inFile, header=TRUE)
x = res$totalDuration
x = x / 1000

startPng(outFile)
hist(x, breaks=25, xlim=c(min(x), max(x)/2), main="Histogram of total time spend in middleware")
# lines(density(x), col="blue", lwd=2) # add a density estimate with defaults

mn <- mean(x)
md <- median(x)
abline(v = mn, col = "red", lwd = 2)
abline(v = md, col = "green", lwd = 2)

my.legend.size <- legend("topright"
  , c("mean", "median")
  , lty=c(1,1) # gives the legend appropriate symbols (lines)
  , lwd=c(2.5,2.5)
  , col=c("red", "green")) # gives the legend lines the correct color and width
