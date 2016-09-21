#!/bin/bash

# Abort on error
set -e

./scripts/code_health.sh
./scripts/build-build.sh
./scripts/install-build.sh
./scripts/build.sh
./scripts/test.sh
./scripts/lines.sh
