.PHONY: build clean repl watch ;\
	cic ci formatc format lint lintc ;\
	haddock hackage outdated

PKGS := \
	effects-async \
	effects-env \
	effects-exceptions \
	effects-fs \
	effects-ioref \
	effects-logger-ns \
	effects-optparse \
	effects-stm \
	effects-terminal \
	effects-thread \
	effects-time \
	effects-typed-process

# core

T = ""

build:
	if [ -z "$(T)" ]; then \
		cabal build all; \
	else \
		cabal build $(T); \
	fi

clean:
	cabal clean

repl:
	cabal repl $(T)

watch:
	ghcid --command "cabal repl $(T)"

# ci

cic: formatc lintc

ci: lint format

# formatting

EXCLUDE_BUILD := ! -path "./.*" ! -path "./*dist-newstyle/*" ! -path "./*stack-work/*"
FIND_HS := find . -type f -name "*hs" $(EXCLUDE_BUILD)
FIND_CABAL := find . -type f -name "*.cabal" $(EXCLUDE_BUILD)

formatc:
	nixpkgs-fmt ./ --check && \
	$(FIND_CABAL) | xargs cabal-fmt --check && \
	$(FIND_HS) | xargs ormolu --mode check

format:
	nixpkgs-fmt ./ && \
	$(FIND_CABAL) | xargs cabal-fmt --inplace && \
	$(FIND_HS) | xargs ormolu -i

# linting

lint:
	$(FIND_HS) | xargs -I % sh -c " \
		hlint \
		--ignore-glob=dist-newstyle \
		--ignore-glob=stack-work \
		--refactor \
		--with-refactor=refactor \
		--refactor-options=-i \
		%"

lintc:
	hlint . --ignore-glob=dist-newstyle --ignore-glob=stack-work

# generate docs for main package, copy to docs/
haddock:
	set -e ;\
	cabal haddock all --haddock-hyperlink-source --haddock-quickjump --project-file cabal.project.legacy ;\
	mkdir -p docs/ ;\

	for p in $(PKGS); do \
		rm -rf docs/$$p* ;\
		cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/$$p-0.1/doc/html/* docs/ ;\
	done

hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc

outdated:
	for p in $(PKGS); do \
		echo $$p ;\
		cd $$p ;\
		cabal outdated ;\
		echo "" ;\
		cd ../ ;\
	done