startPng <- function(file) {
  pngFile <- paste(file, ".png", sep="")
  png(pngFile, height=300, width=600, bg="white")
}
