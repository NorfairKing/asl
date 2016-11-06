library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 2) {
  stop("Usage analyze_trace_slice.r <input.csv> <output.png>")
}

inFile <- args[1] 
outFile <- args[2]

startPng <- function(file) {
    png(file, height=450, width=900, bg="white")
    base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
    par(bg=base2)
}

startPng(outFile)

res = read.csv(inFile, header=TRUE)

at <- res$aTime
cat <- res$category
val <- res$value
d <- data.frame(at, cat, val)

gg <- ggplot(d, aes(x=at, y=val))
gg <- gg + geom_area(aes(colour=cat, fill=cat))
gg
