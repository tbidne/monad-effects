set -e

export LANG="C.UTF-8"

export dirs="effects-*"

# shellcheck disable=SC2046,SC2086
hlint $(find $dirs -type f -name "*.hs")