{ mkDerivation, base, call-stack, stdenv, tasty }:
mkDerivation {
  pname = "tasty-hunit";
  version = "0.10";
  sha256 = "7cf9f0e4b2bb69e2fc813d941a48b603bded7128f13dbd48034b6d02b7d7ed20";
  libraryHaskellDepends = [ base call-stack tasty ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "HUnit support for the Tasty test framework";
  license = stdenv.lib.licenses.mit;
}
