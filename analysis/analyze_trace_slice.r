library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 4) {
  stop("Usage analyze_trace_slice.r <common.r> <input.csv> <outputprefix> <absolute|relative>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]
postfix <- args[4]

if (postfix != 'absolute' && postfix != 'relative') {
  stop("Postfix must be 'absolute' or 'relative'")
}

isabs <- postfix == 'absolute'

xl = "Elapsed time (seconds)"

if(isabs){
  title = "Absolute time spent in middleware per request"
  yl = "Average absolute time spent in middleware (microseconds)"
} else {
  title = "Relative time spent in middleware per request"
  yl = "Average relative time spent in middleware (percentage)"
}

startPng(paste(outFile, "slice", sep="-"))

res = read.csv(inFile, header=TRUE)

at <- res$aTime / 1000 / 1000 / 1000 # Convert to seconds
cat <- res$category
val <- res$value
if(isabs) { val <- val / 1000 } # Convert to microseconds

d <- data.frame(at, cat, val)

d$cat <- factor(d$cat, levels=cat[1:6])

gg <- ggplot(d, aes(x=at, y=val, fill=cat))
gg <- gg + geom_area(aes(colour=cat, fill=cat))
gg <- gg + ggtitle(title) + xlab(xl) + ylab(yl)
gg <- gg + theme(legend.title=element_blank())
gg
