source scripts/lib.sh
h () {
  hlint \
    asl-build/src
}
check "Hlint" h
