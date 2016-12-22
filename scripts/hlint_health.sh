#!/bin/bash
source scripts/lib.sh
h () {
  hlint \
    orchestra/src \
    --ignore "Eta reduce" \
    --ignore "Reduce duplication"
}
check "Hlint" h
