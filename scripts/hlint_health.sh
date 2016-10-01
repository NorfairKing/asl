#!/bin/bash
source scripts/lib.sh
h () {
  hlint \
    asl-build/src \
    --ignore "Eta reduce"
}
check "Hlint" h
