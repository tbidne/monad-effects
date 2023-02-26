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

formatc:
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode check ;\
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt -- --check

format:
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.8#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.8#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock all --haddock-hyperlink-source --haddock-quickjump ;\
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