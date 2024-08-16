set -e

export LANG="C.UTF-8"

cabal bench fs-unix:benchmark:benchmarks --benchmark-options \
    '+RTS -T -RTS -t100
    --baseline bench/baseline_9.8.2.csv
    --csv bench/baseline_9.8.2.csv
    --svg bench/baseline_9.8.2.svg
    --fail-if-faster 30
    --fail-if-slower 30'
