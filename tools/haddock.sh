set -e

export LANG="C.UTF-8"

export dirs="effects-*"

# shellcheck disable=SC2086
cabal haddock all --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
find docs/* ! -path docs/index.html ! -path docs/linuwial.css | xargs -I % sh -c "rm -r %"

for d in $dirs; do
  # shellcheck disable=SC2086
  rm -rf docs/$d*
  # shellcheck disable=SC2086
  cp -r dist-newstyle/build/x86_64-linux/ghc-9.6.2/$d-0.1/doc/html/* docs/
done