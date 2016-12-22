library(R.matlab)
library(RJSONIO)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 12) {
    stop("Usage analyze_mymodel.r <common.r> <tmpResultPath> <outResultPath> <nrServers> <nrThreads> <writeProp> <arrivalRate> <arrivalRate> <acceptorServiceTime> <readServiceTime> <writeServiceTime> <writeDelayTime> <writeResponseTime>")
}

common <- args[1]
source(common)

tmpResultPath         <- args[2]
outResultPath         <- args[3]
nrServers             <- args[4]
nrThreads             <- args[5]
writeProp             <- args[6]
arrivalRate           <- args[7]
acceptorServiceTime   <- args[8]
readServiceTime       <- args[9]
writeServiceTime      <- args[10]
writeDelayTime        <- args[11]
writeResponseTime     <- args[12]

cmd = paste(
      "octave"
    , "analysis/octave_in.oct"
    , tmpResultPath
    , nrServers
    , nrThreads
    , writeProp 
    , arrivalRate
    , acceptorServiceTime 
    , readServiceTime     
    , writeServiceTime    
    , writeDelayTime      
    , writeResponseTime
    )

system(cmd)
matdata <- readMat(tmpResultPath)
print(matdata)

utilisations = matdata$U[1,]
responseTimes = matdata$R[1,]
nrRequests = matdata$Q[1,]
throughputs = matdata$X[1,]
nrVisits = matdata$V[1,]
serviceTimes = matdata$S[,1]

if (responseTimes == serviceTimes) { print("Wut") }

totalResponseTime = sum(nrVisits %*% responseTimes)
avgNrRequests = sum(nrRequests)

json <- structure(
    list(totalResponseTime, avgNrRequests, utilisations, responseTimes, throughputs, nrRequests, nrVisits, serviceTimes)
  , .Names=c("totalResponseTime", "avgNrRequests", "utilisations", "responseTimes", "throughputs", "nrRequests", "nrVisits", "serviceTimes")
  )
contents <- toJSON(json)
write(contents, outResultPath)
