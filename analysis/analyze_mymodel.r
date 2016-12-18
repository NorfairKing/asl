library(R.matlab)

print("BEGIN OF OCTAVE OUTPUT")
system("octave analysis/octave_in.oct")
matdata <- readMat("/tmp/octave_out.mat")
print("END OF OCTAVE OUTPUT")
# print(matdata)
