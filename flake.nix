{
  description = "A Collection of Monadic Effects";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    { flake-parts
    , self
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
              # These tests seems to hang, see:
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hsOverlay =
            (compiler.extend (hlib.compose.packageSourceOverrides {
              monad-callstack = ./monad-callstack;
              monad-fs = ./monad-fs;
              monad-ioref = ./monad-ioref;
              monad-logger-namespace = ./monad-logger-namespace;
              monad-stm = ./monad-stm;
              monad-terminal = ./monad-terminal;
              monad-thread = ./monad-thread;
              monad-time = ./monad-time;
            }));
          packages = p: [
            p.monad-callstack
            p.monad-fs
            p.monad-ioref
            p.monad-logger-namespace
            p.monad-stm
            p.monad-terminal
            p.monad-thread
            p.monad-time
          ];
        in
        {
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
