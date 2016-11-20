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

xl = "Virtual clients (no unit)"

if(isabs){
  yl = "Average absolute time spent in middleware (milliseconds)"
} else {
  yl = "Average relative time spent in middleware (percentage)"
}



res = read.csv(inFile, header=TRUE)
if(isabs) { res$value <- res$value / 1000 / 1000 } # Convert to milliseconds

totals = res[res$category == "totalTime", ]
maxTime = max(totals$value)

res <- res[res$category != "totalTime",]

for (threads in unique(res$middleThreads)) {
  file = paste(outFile, threads, "slice", sep="-")
  startPng(file)
  if (isabs) {
    title = paste("Absolute time spent in middleware per request (", threads, " threads per pool)", sep="")
  } else {
    title = paste("Relative time spent in middleware per request (", threads, " threads per pool)", sep="")
  }

  dat <- res[res$middleThreads == threads,]

  nrc <- dat$nrClients
  cat <- dat$category
  val <- dat$value

  d <- data.frame(nrc, cat, val)

  # A nicer order
  d$cat <- factor(d$cat, levels=rev(cat[1:6]))

  gg <- ggplot(d, aes(x=nrc, y=val, fill=cat))
  gg <- gg + geom_bar(stat='identity')
  gg <- gg + ggtitle(title) + xlab(xl) + ylab(yl)
  gg <- gg + coord_cartesian(ylim=c(0,maxTime))
  gg <- gg + theme(legend.title=element_blank())
  gg <- gg + scale_fill_brewer(palette = "Set1")
  gg
  print(gg)
}
