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
  inputs.exception-utils = {
    url = "github:tbidne/exception-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.fs-utils = {
    url = "github:tbidne/fs-utils";
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
          ghc-version = "ghc9122";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                Cabal-syntax_3_10_3_0 = hlib.doJailbreak prev.Cabal-syntax_3_10_3_0;
                haskeline = prev.haskeline_0_8_4_0;
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "smart-math"
              ];
          };
          pkgsCompiler = {
            inherit pkgs compiler;
          };
          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };
          hsOverlay = (
            compiler.extend (
              hlib.compose.packageSourceOverrides {
                effects-async = ./lib/effects-async;
                effects-env = ./lib/effects-env;
                effects-evaluate = ./lib/effects-evaluate;
                effects-fs = ./lib/effects-fs;
                effects-haskeline = ./lib/effects-haskeline;
                effects-ioref = ./lib/effects-ioref;
                effects-logger = ./lib/effects-logger;
                effects-optparse = ./lib/effects-optparse;
                effects-process = ./lib/effects-process;
                effects-stm = ./lib/effects-stm;
                effects-terminal = ./lib/effects-terminal;
                effects-thread = ./lib/effects-thread;
                effects-time = ./lib/effects-time;
                effects-typed-process = ./lib/effects-typed-process;
                effects-unix = ./lib/effects-unix;
                effects-unix-compat = ./lib/effects-unix-compat;
              }
            )
          );
          packages = p: [
            p.effects-async
            p.effects-env
            p.effects-evaluate
            p.effects-fs
            p.effects-haskeline
            p.effects-ioref
            p.effects-logger
            p.effects-optparse
            p.effects-process
            p.effects-stm
            p.effects-terminal
            p.effects-thread
            p.effects-time
            p.effects-typed-process
            p.effects-unix
            p.effects-unix-compat
          ];

          mkPkg =
            name: root: source-overrides:
            compiler.developPackage {
              inherit name root source-overrides;
              returnShellEnv = false;
            };
        in
        {
          packages.effects-async = mkPkg "effects-async" ./lib/effects-async {
            effects-thread = ./lib/effects-thread;
          };
          packages.effects-env = mkPkg "effects-env" ./lib/effects-env { };
          packages.effects-evaluate = mkPkg "effects-evaluate" ./lib/effects-evaluate { };
          packages.effects-fs = mkPkg "effects-fs" ./lib/effects-fs {
            effects-ioref = ./lib/effects-ioref;
          };
          packages.effects-haskeline = mkPkg "effects-haskeline" ./lib/effects-haskeline { };
          packages.effects-ioref = mkPkg "effects-ioref" ./lib/effects-ioref { };
          packages.effects-logger = mkPkg "effects-logger" ./lib/effects-logger {
            effects-thread = ./lib/effects-thread;
            effects-time = ./lib/effects-time;
          };
          packages.effects-optparse = mkPkg "effects-optparse" ./lib/effects-optparse { };
          packages.effects-process = mkPkg "effects-process" ./lib/effects-process { };
          packages.effects-stm = mkPkg "effects-stm" ./lib/effects-stm { };
          packages.effects-terminal = mkPkg "effects-terminal" ./lib/effects-terminal { };
          packages.effects-time = mkPkg "effects-time" ./lib/effects-time { };
          packages.effects-thread = mkPkg "effects-thread" ./lib/effects-thread { };
          packages.effects-typed-process = mkPkg "effects-typed-process" ./lib/effects-typed-process { };
          packages.effects-unix = mkPkg "effects-unix" ./lib/effects-unix { };
          packages.effects-unix-compat = mkPkg "effects-unix-compat" ./lib/effects-unix-compat { };

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs = [
              (hlib.dontCheck compiler.cabal-fmt)
              (hlib.dontCheck compiler.haskell-language-server)
              pkgs.nixfmt-rfc-style
            ]
            ++ (nix-hs-utils.mkBuildTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (pkgsCompiler // pkgsMkDrv))
                (nix-hs-utils.format-yaml pkgsMkDrv)
              ];
            };

            lint = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.lint (pkgsCompiler // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            lint-refactor = nix-hs-utils.lint-refactor pkgsCompiler;
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
