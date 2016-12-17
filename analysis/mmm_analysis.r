args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("Usage mmm_analysis.r <common.r> <input.csv> <output.png>")
}

common <- args[1]
source(common)
inFile <- args[2] 
outFile <- args[3]

res = read.csv(inFile, header=TRUE, colClasses="numeric")
print(res)

resp = res$respavgavg

startPng(outFile)
plot(
    NULL
  , NULL
  , type='n'
  , xlim=c(0,1)
  , ylim=c(0,max(resp))
  , xlab="Utilisation rate"
  , ylab="Response time (microseconds)"
  )

colors = c("red", "blue", "green")

for (i in (1:nrow(res))) {
  rhos = seq(from=0, to=0.95, by=0.01)
  mu = res$serviceRate[i]
  m = res$nrServers[i]
  r = (1 / mu) * (1 + (rhos / (m * (1 - rhos)))) * 1000 * 1000
  
  c = colors[(i %% length(colors)) + 1]
  lines(rhos, r, col=c)
}
points(res$trafficIntensity, resp)
