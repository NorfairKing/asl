library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Usage analyze_replication_effect.r <common.r> <input.csv>")
}

common <- args[1]
source(common)
inFile <- args[2]
prefix <- args[3]

res = read.csv(inFile, header=TRUE)

xl = "Replication factor"
yl = "Response time (microseconds)"

for (nrSers in unique(res$nrServers)) {
  dat <- res[res$nrServers == nrSers,]

  title = paste("Response time,", nrSers, "servers")
  startPng(paste(prefix, nrSers, sep="-"))

  kind <- dat$kind
  xval <- dat$replicationFactor
  yval <- dat$avg

  d <- data.frame(kind, xval, yval)

  gg <- ggplot(d, aes(factor(xval), yval, fill = kind))
  gg <- gg + ggtitle(title) + xlab(xl) + ylab(yl)
  gg <- gg + theme(legend.title=element_blank())
  gg <- gg + geom_bar(stat="identity", position = "dodge")
  gg <- gg + scale_fill_brewer(palette = "Set1")
  print(gg)
}
