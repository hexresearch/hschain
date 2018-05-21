{ mkDerivation, base, basement, gauge, ghc-prim, stdenv }:
mkDerivation {
  pname = "foundation";
  version = "0.0.20";
  sha256 = "ba6ae63a9ce0846bf942af2c3ace56600f051c61e83a0b55dd625de23a78e42d";
  libraryHaskellDepends = [ base basement ghc-prim ];
  testHaskellDepends = [ base basement ];
  benchmarkHaskellDepends = [ base basement gauge ];
  homepage = "https://github.com/haskell-foundation/foundation";
  description = "Alternative prelude with batteries and no dependencies";
  license = stdenv.lib.licenses.bsd3;
}
