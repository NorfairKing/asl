library(ggplot2)
library(reshape2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage mmm_analysis.r <common.r> <input.csv> <output.png>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outPrefix <- args[3]

res = read.csv(inFile, header=TRUE, colClasses="numeric")

byNrSersFile = paste(outPrefix, "bynrsers", sep="-")
startPng(byNrSersFile)

df <- data.frame(
        nrServers = res$numberOfMemcacheds 
      , replicationFactor = res$replicationFactor
      , modelResp = res$meanResponseTime * 1000 * 1000
      , realResp = res$respavgavg
      )
df.long <- melt(df, id.vars = c("nrServers", "replicationFactor"))

gg <- ggplot(df.long)
gg <- gg + geom_bar(aes(x = factor(replicationFactor), y = value, fill=variable), stat = "identity", position="dodge")
gg <- gg + facet_grid (~ nrServers, scales="free_x")
gg <- gg + ggtitle("Response time by number of servers") + xlab("Replication factor") + ylab("Response time μs")
gg <- gg + scale_fill_discrete(name="Kind", labels=c("Model", "Real"))
print(gg)

byRepCofFile = paste(outPrefix, "byrepcof", sep="-")
startPng(byRepCofFile)

df <- data.frame(
        nrServers = res$numberOfMemcacheds 
      , replicationCoefficient = res$replicationCoeff
      , modelResp = res$meanResponseTime * 1000 * 1000
      , realResp = res$respavgavg
      )
df.long <- melt(df, id.vars = c("nrServers", "replicationCoefficient"))

gg <- ggplot(df.long)
gg <- gg + geom_bar(aes(x = factor(nrServers), y = value, fill=variable), stat = "identity", position="dodge")
gg <- gg + facet_grid (~ replicationCoefficient, scales="free_x")
gg <- gg + ggtitle("Response time by replication coefficient") + xlab("number of servers") + ylab("Response time μs")
gg <- gg + scale_fill_discrete(name="Kind", labels=c("Model", "Real"))
print(gg)
