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
yl = "Average response time (milliseconds)"

res$avg <- res$avg / 1000
res$std <- res$std / 1000

maxTime = max(res$avg + res$std)

# TODO error bars
for (nrSers in unique(res$nrServers)) {
  dat <- res[res$nrServers == nrSers,]

  title = paste("Response time,", nrSers, "servers")
  startPng(paste(prefix, "id", nrSers, sep="-"))

  kind <- dat$kind
  xval <- dat$replicationFactor
  yval <- dat$avg
  err <- dat$std

  d <- data.frame(kind, xval, yval)

  gg <- ggplot(d, aes(factor(xval), yval, fill = kind))
  gg <- gg + geom_bar(stat="identity", position = "dodge")
  gg <- gg + geom_errorbar(aes(ymin=yval-err, ymax=yval+err), width=.2,position=position_dodge(.9))
  gg <- gg + ggtitle(title) + xlab(xl) + ylab(yl)
  gg <- gg + coord_cartesian(ylim=c(0,maxTime))
  gg <- gg + theme(legend.title=element_blank())
  gg <- gg + scale_fill_brewer(palette = "Set1")
  print(gg)
}

for (repcof in unique(res$replicationCoefficient)) {
  dat <- res[res$replicationCoefficient == repcof,]

  title = paste("Response time, R/S=", repcof)
  startPng(paste(prefix, "rev", repcof * 2, sep="-"))

  kind <- dat$kind
  xval <- dat$nrServers
  yval <- dat$avg
  err <- dat$std

  d <- data.frame(kind, xval, yval)

  gg <- ggplot(d, aes(factor(xval), yval, fill = kind))
  gg <- gg + geom_bar(stat="identity", position = "dodge")
  gg <- gg + geom_errorbar(aes(ymin=yval-err, ymax=yval+err), width=.2,position=position_dodge(.9))
  gg <- gg + ggtitle(title) + xlab(xl) + ylab(yl)
  gg <- gg + coord_cartesian(ylim=c(0,maxTime))
  gg <- gg + theme(legend.title=element_blank())
  gg <- gg + scale_fill_brewer(palette = "Set1")
  print(gg)
}
