library(igraph)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 1) {
  stop("Input CSV filepath expected as first argument")
}

if (length(args) < 2) {
  stop("Output plot filepaths expected as second argument")
}

resFile <- args[1] # First argument
filePrefix <- args[2]
outFileTps <- paste(filePrefix, "tps", sep="-")
outFileAvg <- paste(filePrefix, "avg", sep="-")

res = read.csv(resFile, header=TRUE)

startPng <- function(file) {
  png(filename=file, height=600, width=800, bg="white")
  base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
  par(
      bg=base2#adjustcolor("lightgreen", alpha.f=0.3)
    )
}

sign <- function (file, curNrClients) {
  paste(file, "-", curNrClients, ".png", sep="")
}

maxNrClients = max(res$nrClients)
maxNrRepetitions = max(res$rep)

for (curNrClients in 1:maxNrClients) {
  data = res[res$nrClients == curNrClients, ]

  # Get a list of values to key on.
  concurrencies <- unique(data$concurrency)
  nrTpsVals = length(concurrencies) * maxNrRepetitions
  tpsCombined = matrix(0, nrow=nrTpsVals, ncol=2) # Concurrency, sum of tps
  colnames(tpsCombined) <- c("concurrency", "tps")

  rowNr = 0
  for (conc in concurrencies) {
    for (rep in 1:maxNrRepetitions) {
      rowNr = rowNr + 1
      tpsCombined[rowNr, 1] = conc
      toSum = data$tps[data$concurrency == conc & data$rep == rep]
      tpsSum = sum(toSum)
      tpsCombined[rowNr, 2] = tpsSum
    }
  }
  # Turn it into a data frame.
  tpsCombined = as.data.frame(tpsCombined)

  startPng(sign(outFileTps, curNrClients))
  plot(
      tpsCombined$concurrency
    , tpsCombined$tps
    , main=paste("Aggregated throughput", curNrClients, "clients")
    , xlab="log(Virtual clients) (no unit)"
    , ylab="Throughput (Operations/second)"
    , log="x"
    , bg="green"
    )

  startPng(sign(outFileAvg, curNrClients))
  plot(
      data$concurrency, data$avg
    , main=paste("Aggregated response time", curNrClients, "clients")
    , xlab="Virtual clients (no unit)"
    , ylab="Average response time"
    )

  # hack: we draw arrows but with very special "arrowheads"
  # No arrowheads with wide line: bars
  arrows(
      data$concurrency
    , data$avg-data$std
    , data$concurrency
    , data$avg+data$std
    , length=0 # Length of arrow head
    , lwd=20 # Line width
    , col=adjustcolor("darkblue", alpha.f=0.05)
    )
  points(
      data$concurrency, data$avg
    , col="red"
    , pch=19 # Point shape: filled circle.
    )
}


