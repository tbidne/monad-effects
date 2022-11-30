{ stack-vers ? "default"
, dev ? false
}:

let
  pkgs = import ((import ../default.nix).inputs.nixpkgs) { };
  to-keys = pkgs.lib.attrsets.mapAttrsToList (k: v: k);
  strings = pkgs.lib.strings;
  show-keys = attrs:
    strings.concatStrings
      (strings.intersperse ", " (to-keys attrs));

  stack-map = {
    lts-18 = { ghc = "ghc8107"; yaml = "stack_lts-18.yaml"; };
    lts-19 = { ghc = "ghc902"; yaml = "stack_lts-19.yaml"; };
    default = { ghc = "ghc925"; yaml = "stack.yaml"; };
    nightly = { ghc = "ghc943"; yaml = "stack_nightly.yaml"; };
  };
  val =
    if stack-map ? ${stack-vers}
    then stack-map.${stack-vers}
    else
      throw
        ''Invalid key: '${stack-vers}'. Valid keys are: ${show-keys stack-map}
        '';
  compiler = pkgs.haskell.packages.${val.ghc};

  dev-tools = [
    compiler.haskell-language-server
  ];
in
pkgs.haskell.lib.buildStackProject {
  name = "monad-effects";

  buildInputs = with pkgs; [
    git
    stack
    zlib.dev
    zlib.out
  ] ++ (if dev then dev-tools else [ ]);

  ghc = compiler.ghc;

  STACK_YAML = "${val.yaml}";
}
