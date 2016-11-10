startPng <- function(file) {
    png(paste(file, ".png", sep=""), height=450, width=900, bg="white")
    base2 = rgb(0xB5/256, 0x89/256, 0x00/256, 0.1)
    par(bg=base2)
  }
