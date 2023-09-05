{
  description = "A Collection of Monadic Effects";

  # nix
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.smart-math = {
    url = "github:tbidne/smart-math";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
  };
  outputs =
    inputs@{ flake-parts
    , nix-hs-utils
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc962";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              file-io = final.callHackage "file-io" "0.1.0.1" { };
              hedgehog = prev.hedgehog_1_3;
              hlint = prev.hlint_3_6_1;
              ormolu = prev.ormolu_0_7_1_0;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
              "smart-math"
            ];
          };
          pkgsCompiler = { inherit pkgs compiler; };
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
              effects-unix-compat = ./effects-unix-compat;
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
            p.effects-unix-compat
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
          packages.effects-fs = compiler.developPackage {
            name = "effects-fs";
            root = ./effects-fs;
            returnShellEnv = false;
            source-overrides = {
              effects-exceptions = ./effects-exceptions;
              effects-ioref = ./effects-ioref;
            };
          };
          packages.effects-ioref = mkPkgsException "effects-ioref" ./effects-ioref;
          packages.effects-logger-ns =
            mkPkg "effects-logger-ns" ./effects-logger-ns {
              effects-exceptions = ./effects-exceptions;
              effects-time = ./effects-time;
            };
          packages.effects-optparse =
            mkPkg "effects-optparse" ./effects-optparse {
              effects-exceptions = ./effects-exceptions;
              effects-fs = ./effects-fs;
              effects-ioref = ./effects-ioref;
            };
          packages.effects-stm = mkPkgsException "effects-stm" ./effects-stm;
          packages.effects-time = mkPkgsException "effects-time" ./effects-time;
          packages.effects-terminal = mkPkgsException "effects-terminal" ./effects-terminal;
          packages.effects-thread = mkPkgsException "effects-thread" ./effects-thread;
          packages.effects-typed-process =
            mkPkg "effects-typed-process" ./effects-typed-process {
              effects-exceptions = ./effects-exceptions;
              effects-stm = ./effects-stm;
            };
          packages.effects-unix-compat = mkPkg "effects-unix-compat" ./effects-unix-compat { };

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs =
              (nix-hs-utils.mkBuildTools pkgsCompiler)
              ++ (nix-hs-utils.mkDevTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.format pkgsCompiler;
            lint = nix-hs-utils.lint pkgsCompiler;
            lint-refactor = nix-hs-utils.lint-refactor pkgsCompiler;
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
