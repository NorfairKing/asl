set -e # abort on error

./scripts/code_health.sh
./scripts/code_health.sh
./scripts/build-build.sh
./scripts/install-build.sh
./scripts/documentation.sh
./scripts/test.sh
./scripts/lines.sh
