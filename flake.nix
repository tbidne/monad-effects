{
  description = "A Collection of Monadic Effects";

  # nix
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.smart-math = {
    url = "github:tbidne/smart-math";
    inputs.flake-compat.follows = "flake-compat";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
  };
  outputs =
    { algebra-simple
    , bounds
    , flake-parts
    , self
    , smart-math
    , ...
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc925";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
              bounds = final.callCabal2nix "bounds" bounds { };
              # These tests seems to hang, see:
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
              hedgehog = prev.hedgehog_1_2;
              smart-math = final.callCabal2nix "smart-math" smart-math { };
              tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
            };
          };
          hsOverlay =
            (compiler.extend (hlib.compose.packageSourceOverrides {
              monad-async = ./monad-async;
              monad-env = ./monad-env;
              monad-exceptions = ./monad-exceptions;
              monad-fs = ./monad-fs;
              monad-ioref = ./monad-ioref;
              monad-logger-namespace = ./monad-logger-namespace;
              monad-optparse = ./monad-optparse;
              monad-stm = ./monad-stm;
              monad-system-time = ./monad-system-time;
              monad-terminal = ./monad-terminal;
              monad-thread = ./monad-thread;
              monad-typed-process = ./monad-typed-process;
            }));
          packages = p: [
            p.monad-async
            p.monad-env
            p.monad-exceptions
            p.monad-fs
            p.monad-ioref
            p.monad-logger-namespace
            p.monad-optparse
            p.monad-stm
            p.monad-system-time
            p.monad-terminal
            p.monad-thread
            p.monad-typed-process
          ];

          mkPkg = name: root: source-overrides: compiler.developPackage {
            inherit name root source-overrides;
            returnShellEnv = false;
          };
          mkPkgsException = name: root: mkPkg name root {
            monad-exceptions = ./monad-exceptions;
          };
        in
        {
          packages.monad-async =
            mkPkg "monad-async" ./monad-async {
              monad-exceptions = ./monad-exceptions;
              monad-ioref = ./monad-ioref;
              monad-stm = ./monad-stm;
              monad-thread = ./monad-thread;
            };
          packages.monad-env = mkPkgsException "monad-env" ./monad-env;
          packages.monad-exceptions = mkPkg "monad-exceptions" ./monad-exceptions { };
          packages.monad-fs = mkPkgsException "monad-fs" ./monad-fs;
          packages.monad-ioref = mkPkgsException "monad-ioref" ./monad-ioref;
          packages.monad-logger-namespace =
            mkPkg "monad-logger-namespace" ./monad-logger-namespace {
              monad-exceptions = ./monad-exceptions;
              monad-system-time = ./monad-system-time;
            };
          packages.monad-optparse = mkPkgsException "monad-optparse" ./monad-optparse;
          packages.monad-stm = mkPkgsException "monad-stm" ./monad-stm;
          packages.monad-system-time = mkPkgsException "monad-system-time" ./monad-system-time;
          packages.monad-terminal = mkPkgsException "monad-terminal" ./monad-terminal;
          packages.monad-thread = mkPkgsException "monad-thread" ./monad-thread;
          packages.monad-typed-process =
            mkPkg "monad-typed-process" ./monad-typed-process {
              monad-exceptions = ./monad-exceptions;
              monad-stm = ./monad-stm;
            };

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs = (buildTools compiler) ++ (devTools compiler);
          };
          devShells.ci = hsOverlay.shellFor {
            inherit packages;
            withHoogle = false;
            buildInputs = buildTools compiler;
          };
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
