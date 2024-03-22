{ mkDerivation, base, lib, mtl, servant, servant-server, unliftio
}:
mkDerivation {
  pname = "unliftio-servant-server";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base mtl servant servant-server unliftio
  ];
  homepage = "https://github.com/bellroy/haskell/tree/master/lib/unliftio-servant-server";
  license = lib.licenses.bsd3;
}
