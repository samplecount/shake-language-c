{ mkDerivation, base, data-default-class, directory, doctest
, fclabels, hspec, lib, process, shake, split, unordered-containers
}:
mkDerivation {
  pname = "shake-language-c";
  version = "0.13.1";
  src = ./.;
  libraryHaskellDepends = [
    base data-default-class fclabels process shake split
    unordered-containers
  ];
  testHaskellDepends = [ base directory doctest hspec shake ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/samplecount/shake-language-c";
  description = "Utilities for cross-compiling with Shake";
  license = lib.licenses.asl20;
}
