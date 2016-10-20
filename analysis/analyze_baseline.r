library(igraph)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage analyze_baseline.r <resultsfile> <plotprefix> <outputdir>")
}

resFile <- args[1]
filePrefix <- args[2]
outDir <- args[3]
outFileTps <- paste(filePrefix, "tps", sep="-")
outFileAvg <- paste(filePrefix, "avg", sep="-")

res = read.csv(resFile, header=TRUE)

startPng <- function(file) {
  png(filename=paste(outDir, file, sep="/"), height=450, width=800, bg="white")
  base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
  par(bg=base2)
}

# sign <- function (file, curNrClients) {
#   paste(file, "-", curNrClients, ".png", sep="")
# }

maxNrClients = max(res$nrClients)
maxNrRepetitions = max(res$rep)

combineTps <- function(data) {
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
  return(tpsCombined)
}

tpsCombined = combineTps(res)

startPng(paste(outFileTps, "png", sep="."))
plot(
    tpsCombined$concurrency
  , tpsCombined$tps
  , main=paste("Aggregated throughput")
  , xlab="Total number of virtual clients (no unit) (log)"
  , ylab="Throughput (transactions/second)"
  , log="x"
  , bg="green"
  , type="n" # Don't plot just yet.
  , xlim=c(1, 128)
  , ylim=c(0, 40000)
  )

colors = c("red", "blue")
for (curNrClients in 1:maxNrClients) {
  tpsCombinedX = combineTps(res[res$nrClients == curNrClients, ])
  points(
      tpsCombinedX$concurrency * curNrClients
    , tpsCombinedX$tps
    , col=colors[curNrClients]
    )
}
legend( 40000
  , c("One client", "Two clients")
  , pch=c(19, 19) # Symbol type
  , col=colors) 

allConcurrencies = c(1, unique(res$concurrency * maxNrClients))
startPng(paste(outFileAvg, "png", sep="."))
plot(
    res$concurrency, res$avg
  , main="Aggregated response time, combined"
  , log="x"
  , xlab="Virtual clients (no unit)"
  , ylab="Average response time (us)"
  , xlim=c(min(allConcurrencies), max(allConcurrencies))
  , ylim=c(min(res$avg-res$std), max(res$avg+res$std))
  , type="n" # Don't plot just yet.
  , xaxt="n" # Don't plot labels on the x axis
  )
axis(1, at = allConcurrencies, las=2)
legend( 8000
  , c("One client", "Two clients")
  , pch=c(4, 4) # Symbol type
  , col=colors) 

side = -0.07
for (curNrClients in 1:maxNrClients) {
  data = res[res$nrClients == curNrClients, ]
  concurrencies <- unique(data$concurrency)

  data$concurrency = data$concurrency * curNrClients * (1 + side)
  # hack: we draw arrows but with very special "arrowheads"
  # No arrowheads with wide line: bars
  arrows(
      data$concurrency
    , data$avg-data$std
    , data$concurrency
    , data$avg+data$std
    , length=0 # Length of arrow head
    , lwd=20 # Line width
    , col=adjustcolor(colors[3-curNrClients], alpha.f=(1/(maxNrRepetitions*1.5)))
    )
  points(
      data$concurrency, data$avg
    , col=colors[curNrClients]
    , pch=4 # Point shape: Cross
    )

  side = - side
}
