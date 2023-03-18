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
    inputs@{ algebra-simple
    , bounds
    , flake-parts
    , self
    , smart-math
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: [
            c.cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: [
            (hlib.dontCheck c.ghcid)
            (hlib.dontCheck c.haskell-language-server)
          ];
          ghc-version = "ghc944";
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
              effects-async = ./effects-async;
              effects-env = ./effects-env;
              effects-exceptions = ./effects-exceptions;
              effects-fs = ./effects-fs;
              effects-ioref = ./effects-ioref;
              effects-logger-ns = ./effects-logger-ns;
              effects-optparse = ./effects-optparse;
              effects-stm = ./effects-stm;
              effects-terminal = ./effects-terminal;
              effects-thread = ./effects-thread;
              effects-time = ./effects-time;
              effects-typed-process = ./effects-typed-process;
            }));
          packages = p: [
            p.effects-async
            p.effects-env
            p.effects-exceptions
            p.effects-fs
            p.effects-ioref
            p.effects-logger-ns
            p.effects-optparse
            p.effects-stm
            p.effects-terminal
            p.effects-thread
            p.effects-time
            p.effects-typed-process
          ];

          mkPkg = name: root: source-overrides: compiler.developPackage {
            inherit name root source-overrides;
            returnShellEnv = false;
          };
          mkPkgsException = name: root: mkPkg name root {
            effects-exceptions = ./effects-exceptions;
          };
        in
        {
          packages.effects-async =
            mkPkg "effects-async" ./effects-async {
              effects-exceptions = ./effects-exceptions;
              effects-ioref = ./effects-ioref;
              effects-stm = ./effects-stm;
              effects-thread = ./effects-thread;
            };
          packages.effects-env = mkPkgsException "effects-env" ./effects-env;
          packages.effects-exceptions = mkPkg "effects-exceptions" ./effects-exceptions { };
          packages.effects-fs =
            mkPkg "effects-fs" ./effects-fs {
              effects-exceptions = ./effects-exceptions;
              effects-ioref = ./effects-ioref;
            };
          packages.effects-ioref = mkPkgsException "effects-ioref" ./effects-ioref;
          packages.effects-logger-ns =
            mkPkg "effects-logger-ns" ./effects-logger-ns {
              effects-exceptions = ./effects-exceptions;
              effects-time = ./effects-time;
            };
          packages.effects-optparse = mkPkgsException "effects-optparse" ./effects-optparse;
          packages.effects-stm = mkPkgsException "effects-stm" ./effects-stm;
          packages.effects-time = mkPkgsException "effects-time" ./effects-time;
          packages.effects-terminal = mkPkgsException "effects-terminal" ./effects-terminal;
          packages.effects-thread = mkPkgsException "effects-thread" ./effects-thread;
          packages.effects-typed-process =
            mkPkg "effects-typed-process" ./effects-typed-process {
              effects-exceptions = ./effects-exceptions;
              effects-stm = ./effects-stm;
            };

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs = (buildTools compiler) ++ (devTools compiler);
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
