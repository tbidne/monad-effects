set -e

export LANG="C.UTF-8"

cabal bench effects-fs:benchmark:benchmarks --benchmark-options \
    '--csv bench/bench.csv --svg bench/bench.svg'