{ mkDerivation, attoparsec, base, bimap, bytestring, containers
, cryptohash, hnix, http-conduit, mtl, optparse-applicative
, prettyprinter, prettyprinter-ansi-terminal, stdenv, tasty-hspec
, text, zip-archive
}:
mkDerivation {
  pname = "jenkinsPlugins2nix";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers cryptohash hnix http-conduit
    mtl prettyprinter text zip-archive
  ];
  executableHaskellDepends = [
    base bimap optparse-applicative prettyprinter-ansi-terminal text
  ];
  testHaskellDepends = [ base containers tasty-hspec text ];
  homepage = "https://github.com/Fuuzetsu/jenkinsPlugins2nix#readme";
  description = "Generate nix for Jenkins plugins";
  license = stdenv.lib.licenses.bsd3;
}
