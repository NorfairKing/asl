library(ggplot2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Usage analyze_replication_cost.r <common.r> <input.csv>")
}

common <- args[1]
source(common)
inFile <- args[2]
prefix <- args[3]

res = read.csv(inFile, header=TRUE)

xl = "Replication factor"
yl = "Response time (milliseconds)"

res$time <- res$time / 1000 / 1000 # Convert to milliseconds
maxTime = max(res$time)

for (nrSers in unique(res$nrServers)) {
  for (knd in unique(res$kind)) {
    file = paste(prefix, nrSers, knd, sep="-")
    startPng(file)

    title = paste("Response time,", nrSers, "servers,", knd, "requests")

    dat <- res[res$nrServers == nrSers,]
    dat <- dat[dat$kind == knd,]

    xval <- dat$replicationFactor
    yval <- dat$time
    cat <- dat$category

    d <- data.frame(xval, yval, cat)
    d$cat <- factor(d$cat, levels=rev(cat[1:6]))

    gg <- ggplot(d, aes(x=factor(xval), y=yval, fill=cat))
    gg <- gg + geom_bar(stat='identity')
    gg <- gg + ggtitle(title) + xlab(xl) + ylab(yl)
    gg <- gg + coord_cartesian(ylim=c(0,maxTime))
    gg <- gg + theme(legend.title=element_blank())
    gg <- gg + scale_fill_brewer(palette = "Set1")
    print(gg)
  }
}
