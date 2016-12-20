library(ggplot2)
library(reshape2)
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage mmm_analysis.r <common.r> <input.csv> <output.png>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]

res = read.csv(inFile, header=TRUE, colClasses="numeric")

startPng(outFile)

df <- data.frame(
        nrServers = res$nrServers 
      , replicationFactor = res$replicationFactor
      , modelResp = res$meanResponseTime * 1000 * 1000 * 10
      , realResp = res$respavgavg
      )

head(df)

df.long <- melt(df, id.vars = c("nrServers", "replicationFactor"))

print(df.long)

gg <- ggplot(df.long)
gg <- gg + geom_bar(aes(x = factor(replicationFactor), y = value, fill=variable), stat = "identity", position="dodge")
gg <- gg + facet_grid (~ nrServers, scales="free_x")
print(gg)
