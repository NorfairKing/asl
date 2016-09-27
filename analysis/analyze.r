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

png(filename=outFileTps1, height=600, width=800, bg="white")
plot(res$concurrency, res$tps
     , main="Aggregated throughput"
     , xlab="Virtual clients (no unit)"
     , ylab="Throughput (Operations/second)")

png(filename=outFileAvg1, height=600, width=800, bg="white")
plot(res$concurrency, res$avg
     , main="Aggregated throughput"
     , xlab="Virtual clients (no unit)"
     , ylab="Average response time")
