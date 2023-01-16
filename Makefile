.PHONY: build clean repl watch ;\
	cic ci formatc format lint lintc haddockc ;\
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

cic: formatc lintc haddockc

ci: lint format

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode check ;\
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt -- --check

format:
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode inplace ;\
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.7#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.7#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock all --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	rm -rf docs/monad-async* ;\
	rm -rf docs/monad-callstack* ;\
	rm -rf docs/monad-fs* ;\
	rm -rf docs/monad-ioref* ;\
	rm -rf docs/monad-logger-namespace* ;\
	rm -rf docs/monad-stm* ;\
	rm -rf docs/monad-system-time* ;\
	rm -rf docs/monad-terminal* ;\
	rm -rf docs/monad-thread* ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-async-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-callstack-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-fs-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-ioref-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-logger-namespace-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-stm-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-system-time-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-terminal-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/monad-thread-0.1/doc/html/* docs/ ;\

haddockc:
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-async \
		-m Effects.MonadAsync 90;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-callstack ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-fs \
		-m Effects.FileSystem.MonadPathReader 85 \
		-m Effects.FileSystem.MonadPathWriter 85 ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-ioref ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-logger-namespace \
		-m Effects.MonadLoggerNamespace 85 ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-stm ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-system-time \
		-m Effects.MonadTime 75 ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-terminal ;\
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- ./monad-thread

.PHONY: hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
