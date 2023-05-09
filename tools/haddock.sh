set -e

export LANG="C.UTF-8"

export dirs="effects-*"

cabal update
# shellcheck disable=SC2086
cabal haddock all --haddock-hyperlink-source --haddock-quickjump --project-file cabal.project.legacy

mkdir -p docs/

# shellcheck disable=SC2038
find docs/ -type f | xargs -I % sh -c "rm -r %"

for d in $dirs; do
  # shellcheck disable=SC2086
  rm -rf docs/$d*
  # shellcheck disable=SC2086
  cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/$d-0.1/doc/html/* docs/
done