{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss/chore/dud260609";
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      supportedCompilers = [
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
        "ghc914"
      ];
      defaultCompiler = "ghc910";
    };
}
