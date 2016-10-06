args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 1) {
  stop("Input CSV filepath expected as first argument")
}

if (length(args) < 3) {
  stop("Output plot filepaths expected as second and third argument")
}

resFile <- args[1] # First argument
outFileTps1 <- args[2]
outFileAvg1 <- args[3]

res = read.csv(resFile, header=TRUE)

startPng <- function(file) {
  png(filename=file, height=600, width=800, bg="white")
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

  startPng(sign(outFileTps1, curNrClients))
  plot(tpsCombined$concurrency, tpsCombined$tps
       , main=paste("Aggregated throughput", curNrClients, "clients")
       , xlab="Virtual clients (no unit)"
       , ylab="Throughput (Operations/second)")

  startPng(sign(outFileAvg1, curNrClients))
  plot(data$concurrency, data$avg
       , main=paste("Aggregated throughput", curNrClients, "clients")
       , xlab="Virtual clients (no unit)"
       , ylab="Average response time")

}


