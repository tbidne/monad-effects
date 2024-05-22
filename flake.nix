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
    inputs@{
      flake-parts,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc982";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              { }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "smart-math"
              ];
          };
          pkgsCompiler = {
            inherit pkgs compiler;
          };
          hsOverlay = (
            compiler.extend (
              hlib.compose.packageSourceOverrides {
                effects-async = ./lib/effects-async;
                effects-env = ./lib/effects-env;
                effects-exceptions = ./lib/effects-exceptions;
                effects-fs = ./lib/effects-fs;
                effects-ioref = ./lib/effects-ioref;
                effects-logger-ns = ./lib/effects-logger-ns;
                effects-optparse = ./lib/effects-optparse;
                effects-stm = ./lib/effects-stm;
                effects-terminal = ./lib/effects-terminal;
                effects-thread = ./lib/effects-thread;
                effects-time = ./lib/effects-time;
                effects-typed-process = ./lib/effects-typed-process;
                effects-unix-compat = ./lib/effects-unix-compat;
              }
            )
          );
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

          mkPkg =
            name: root: source-overrides:
            compiler.developPackage {
              inherit name root source-overrides;
              returnShellEnv = false;
            };
          mkPkgsException = name: root: mkPkg name root { effects-exceptions = ./lib/effects-exceptions; };
        in
        {
          packages.effects-async = mkPkg "effects-async" ./lib/effects-async {
            effects-exceptions = ./lib/effects-exceptions;
            effects-thread = ./lib/effects-thread;
          };
          packages.effects-env = mkPkgsException "effects-env" ./lib/effects-env;
          packages.effects-exceptions = mkPkg "effects-exceptions" ./lib/effects-exceptions { };
          packages.effects-fs = compiler.developPackage {
            name = "effects-fs";
            root = ./lib/effects-fs;
            returnShellEnv = false;
            source-overrides = {
              effects-exceptions = ./lib/effects-exceptions;
              effects-ioref = ./lib/effects-ioref;
              effects-unix-compat = ./lib/effects-unix-compat;
            };
          };
          packages.effects-ioref = mkPkgsException "effects-ioref" ./lib/effects-ioref;
          packages.effects-logger-ns = mkPkg "effects-logger-ns" ./lib/effects-logger-ns {
            effects-exceptions = ./lib/effects-exceptions;
            effects-thread = ./lib/effects-thread;
            effects-time = ./lib/effects-time;
          };
          packages.effects-optparse = mkPkg "effects-optparse" ./lib/effects-optparse {
            effects-exceptions = ./lib/effects-exceptions;
            effects-fs = ./lib/effects-fs;
            effects-ioref = ./lib/effects-ioref;
            effects-unix-compat = ./lib/effects-unix-compat;
          };
          packages.effects-stm = mkPkgsException "effects-stm" ./lib/effects-stm;
          packages.effects-terminal = mkPkgsException "effects-terminal" ./lib/effects-terminal;
          packages.effects-time = mkPkgsException "effects-time" ./lib/effects-time;
          packages.effects-thread = mkPkgsException "effects-thread" ./lib/effects-thread;
          packages.effects-typed-process = mkPkgsException "effects-typed-process" ./lib/effects-typed-process;
          packages.effects-unix-compat = mkPkgsException "effects-unix-compat" ./lib/effects-unix-compat;

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs = (nix-hs-utils.mkBuildTools pkgsCompiler) ++ (nix-hs-utils.mkDevTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.format pkgsCompiler;
            lint = nix-hs-utils.lint pkgsCompiler;
            lintRefactor = nix-hs-utils.lintRefactor pkgsCompiler;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
        "x86_64-linux"
      ];
    };
}
