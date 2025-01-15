set +e

export LANG="C.UTF-8"

export dirs="lib/effects-*"

any_fail=0

for d in $dirs; do
  echo -e "*** TESTING: $d ***\n"
  cd "$d"
  out=$(cabal check)
  ec=$?
  cd ../../

  if [[ $ec != 0 ]]; then
    any_fail=1
    echo -e "\n*** FAILED: $d ***\n"
  fi
done

exit $any_fail
