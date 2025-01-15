set +e

export LANG="C.UTF-8"

export dirs="lib/effects-*"

any_fail=0

for d in $dirs; do
  echo "$d"
  cd "$d"
  out=$(cabal check)
  ec=$?
  echo $out
  echo ""
  cd ../../

  if [[ $ec != 0 ]]; then
    any_fail=1
  fi
done

exit $any_fail
