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

for (nrSers in unique(res$nrServers)) {
  dat <- res[res$nrServers == nrSers,]

  startPng(paste(prefix, nrSers, sep="-"))

  kind <- dat$kind
  xval <- dat$replicationFactor
  yval <- dat$avg

  d <- data.frame(kind, xval, yval)

  gg <- ggplot(d, aes(factor(xval), yval, fill = kind))
  gg <- gg + geom_bar(stat="identity", position = "dodge")
  gg <- gg + scale_fill_brewer(palette = "Set1")
  print(gg)
}
