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
      , realResp = res$respavgavg / 1000
      )
df.long <- melt(df, id.vars = c("nrServers", "replicationFactor"))

gg <- ggplot(df.long)
gg <- gg + geom_bar(aes(x = factor(replicationFactor), y = value, fill=variable), stat = "identity", position="dodge")
gg <- gg + facet_grid (~ nrServers, scales="free_x")
gg <- gg + ggtitle("Response time, grouped by number of servers") + xlab("Replication factor") + ylab("Response time μs")
gg <- gg + scale_fill_discrete(name="Kind", labels=c("Model", "Real"))
print(gg)

byRepCofFile = paste(outPrefix, "byrepcof", sep="-")
startPng(byRepCofFile)

df <- data.frame(
        nrServers = res$numberOfMemcacheds 
      , replicationCoefficient = res$replicationCoeff
      , modelResp = res$meanResponseTime * 1000 * 1000
      , realResp = res$respavgavg / 1000
      )
df.long <- melt(df, id.vars = c("nrServers", "replicationCoefficient"))

gg <- ggplot(df.long)
gg <- gg + geom_bar(aes(x = factor(nrServers), y = value, fill=variable), stat = "identity", position="dodge")
gg <- gg + facet_grid (~ replicationCoefficient, scales="free_x")
gg <- gg + ggtitle("Response time, grouped by replication coefficient") + xlab("number of servers") + ylab("Response time μs")
gg <- gg + scale_fill_discrete(name="Kind", labels=c("Model", "Real"))
print(gg)

absTpsFile = paste(outPrefix, "abstps", sep="-")
startPng(absTpsFile)

df <- data.frame(
        nrServers = res$numberOfMemcacheds
      , replicationCoefficient = res$replicationCoeff
      , throughput = res$tpsavgavg
      , tpsStd = res$tpsavgstd
      )
df$ID<-seq.int(nrow(df))


gg <- ggplot(df, aes(x=factor(nrServers), y = throughput, fill=ID))
gg <- gg + geom_bar(stat="identity", position="dodge")
gg <- gg + geom_errorbar(aes(ymin=throughput - tpsStd, ymax=throughput + tpsStd), width=0.5)
gg <- gg + facet_grid (~ replicationCoefficient, scales="free_x")
gg <- gg + ggtitle("Absulote measured throughput, grouped by Replication Coefficient") + xlab("Number of servers") + ylab("Throughput")
gg <- gg + theme(legend.position="none")
print(gg)

absTpsByNrSersFile = paste(outPrefix, "maxtps", sep="-")
startPng(absTpsByNrSersFile)

df <- data.frame(
        nrServers = res$numberOfMemcacheds
      , replicationFactor = res$replicationFactor
      , maxThroughput = res$maxActualTps
      )
df$ID<-seq.int(nrow(df))


gg <- ggplot(df, aes(x=factor(replicationFactor), y = maxThroughput, fill=ID))
gg <- gg + geom_bar(stat="identity", position="dodge")
gg <- gg + facet_grid (~ nrServers, scales="free_x")
gg <- gg + ggtitle("Maximum measured throughput, grouped by number of servers") + xlab("Number of servers") + ylab("Maximum Throughput")
gg <- gg + theme(legend.position="none")
print(gg)

absRespFile = paste(outPrefix, "absresp", sep="-")
startPng(absRespFile)

df <- data.frame(
        nrServers = res$numberOfMemcacheds
      , replicationCoefficient = res$replicationCoeff
      , responseTime = res$mrespavgavg
      , respStd = res$mrespavgstd
      )
df$ID<-seq.int(nrow(df))


gg <- ggplot(df, aes(x=factor(nrServers), y = responseTime, fill=ID))
gg <- gg + geom_bar(stat="identity", position="dodge")
gg <- gg + geom_errorbar(aes(ymin=responseTime - respStd, ymax=responseTime + respStd), width=0.5)
gg <- gg + facet_grid (~ replicationCoefficient, scales="free_x")
gg <- gg + ggtitle("Absulote measured response time, grouped by Replication Coefficient") + xlab("Number of servers") + ylab("Response time μs")
gg <- gg + theme(legend.position="none")
print(gg)
