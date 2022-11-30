{
  description = "A Template for Haskell Packages";
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
          # add tools like hlint, ormolu, ghcid here if you want them
          # on the PATH
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc924";
          compiler = pkgs.haskell.packages."${ghc-version}";
          hsOverlay =
            (pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
              monad-logger-namespace = ./monad-logger-namespace;
              monad-time = ./monad-time;
            }));
        in
        {
          devShells.default = hsOverlay.shellFor {
            packages = p: [ p.monad-logger-namespace p.monad-time ];
            withHoogle = true;
            buildInputs = (buildTools compiler) ++ (devTools compiler);
          };
          devShells.ci = hsOverlay.shellFor {
            packages = p: [ p.monad-logger-namespace p.monad-time ];
            withHoogle = true;
            buildInputs = (buildTools compiler);
          };
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
