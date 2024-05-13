set -e

export LANG="C.UTF-8"

export dirs="effects-*"

mkdir -p docs/
rm -rf docs/$dirs

# shellcheck disable=SC2086
cabal haddock all --haddock-hyperlink-source --haddock-quickjump

for d in $dirs; do
  # shellcheck disable=SC2086
  rm -rf docs/$d*
  # shellcheck disable=SC2086
  cp -r dist-newstyle/build/x86_64-linux/ghc-*/$d-0.1/doc/html/* docs/
done
