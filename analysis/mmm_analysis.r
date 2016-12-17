args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage mmm_analysis.r <common.r> <input.csv> <output.png>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]

res = read.csv(inFile, header=TRUE, colClasses="numeric")
print(res)

startPng(outFile)
plot(res$meanResponseTime, res$respavgavg)
