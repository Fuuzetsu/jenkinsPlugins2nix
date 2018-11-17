{ mkDerivation, ansi-wl-pprint, attoparsec, base, bimap, bytestring
, containers, cryptohash, hnix, http-conduit, mtl
, optparse-applicative, stdenv, tasty-hspec, text, zip-archive
}:
mkDerivation {
  pname = "jenkinsPlugins2nix";
  version = "0.2.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring containers cryptohash
    hnix http-conduit mtl text zip-archive
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base bimap optparse-applicative text
  ];
  testHaskellDepends = [ base containers tasty-hspec text ];
  homepage = "https://github.com/Fuuzetsu/jenkinsPlugins2nix#readme";
  description = "Generate nix for Jenkins plugins";
  license = stdenv.lib.licenses.bsd3;
}
