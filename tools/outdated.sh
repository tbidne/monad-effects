set -e

export LANG="C.UTF-8"

export dirs="lib/effects-*"

for d in $dirs; do
  echo "$d"
  cd "$d"
  cabal outdated
  echo ""
  cd ../../
done