library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 2) {
  stop("Usage analyze_trace_slice.r <common.r> <input.csv> <outputprefix>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]

startPng(paste(outFile, "slice", sep="-"))

res = read.csv(inFile, header=TRUE)

at <- res$aTime
cat <- res$category
val <- res$value
d <- data.frame(at, cat, val)

gg <- ggplot(d, aes(x=at, y=val))
gg <- gg + geom_area(aes(colour=cat, fill=cat))
gg
