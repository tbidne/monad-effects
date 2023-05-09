set -e

export LANG="C.UTF-8"

export dirs="effects-*"

nixpkgs-fmt ./

# shellcheck disable=SC2046,SC2086
cabal-fmt --inplace $(find $dirs -type f -name '*.cabal')

# shellcheck disable=SC2046,SC2086
ormolu -m inplace $(find $dirs -type f -name '*.hs')