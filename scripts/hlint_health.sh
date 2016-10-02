#!/bin/bash
source scripts/lib.sh
h () {
  hlint \
    orchestra/src \
    --ignore "Eta reduce"
}
check "Hlint" h
