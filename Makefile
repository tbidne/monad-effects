.PHONY: build clean repl watch ;\
	cic ci formatc format lint lintc ;\
	haddock hackage

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
	rm -rf docs/monad-async* ;\
	rm -rf docs/monad-env* ;\
	rm -rf docs/monad-exceptions* ;\
	rm -rf docs/monad-fs* ;\
	rm -rf docs/monad-ioref* ;\
	rm -rf docs/monad-logger-namespace* ;\
	rm -rf docs/monad-optparse* ;\
	rm -rf docs/monad-stm* ;\
	rm -rf docs/monad-system-time* ;\
	rm -rf docs/monad-terminal* ;\
	rm -rf docs/monad-thread* ;\
	rm -rf docs/monad-typed-process* ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-async-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-env-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-exceptions-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-fs-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-ioref-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-logger-namespace-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-optparse-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-stm-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-system-time-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-terminal-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-thread-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/monad-typed-process-0.1/doc/html/* docs/ ;\

.PHONY: hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
