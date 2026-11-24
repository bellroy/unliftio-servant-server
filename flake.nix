{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      supportedCompilers = [
        "ghc98"
        "ghc910"
        "ghc912"
        "ghc914"
      ];
      defaultCompiler = "ghc910";
    };
}
