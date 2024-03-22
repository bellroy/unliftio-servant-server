{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      cabalPackages = [
        {
          name = "unliftio-servant-server";
          path = ./unliftio-servant-server.nix;
        }
      ];
      supportedCompilers = [ "ghc810" "ghc90" "ghc92" "ghc94" "ghc96" ];
      defaultCompiler = "ghc92";
      haskellPackagesOverride = { haskellLib, prev, ... }: {
        servant = haskellLib.doJailbreak prev.servant;
        servant-server = haskellLib.doJailbreak prev.servant-server;
      };
    };
}
