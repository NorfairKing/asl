library(R.matlab)
library(RJSONIO)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 11) {
    stop("Usage analyze_mymodel.r <common.r> <tmpResultPath> <outResultPath> <nrServers> <nrThreads> <writeProp> <arrivalRate> <arrivalRate> <acceptorServiceTime> <readServiceTime> <writeServiceTime> <writeDelayTime>")
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
    )

system(cmd)
matdata <- readMat(tmpResultPath)

utilisations = matdata$U
responseTimes = matdata$R
throughputs = matdata$X

json <- structure(
    list(utilisations, responseTimes, throughputs)
  , .Names=c("utilisations", "responseTimes", "throughputs")
  )
contents <- toJSON(json)
write(contents, outResultPath)
