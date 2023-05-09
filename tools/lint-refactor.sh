set -e

export LANG="C.UTF-8"

export dirs="effects-*"

# shellcheck disable=SC2038,SC2086
find $dirs -type f -name "*.hs" | xargs -I % sh -c " \
		hlint \
		--ignore-glob=dist-newstyle \
		--ignore-glob=stack-work \
		--refactor \
		--with-refactor=refactor \
		--refactor-options=-i \
		%"