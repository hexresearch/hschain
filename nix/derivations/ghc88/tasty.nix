{ mkDerivation, fetchgit
, ansi-terminal, async, base, clock, containers, mtl
, optparse-applicative, stdenv, stm, tagged, unbounded-delays, unix
, wcwidth
}:
mkDerivation {
  pname = "tasty";
  version = "1.2.3";
  src = fetchgit {
    url       = "https://github.com/Shimuuar/tasty";
    rev       = "eb0bbed5996030bb1acfe474dd04056604ce8960";
    sha256    = "sha256:1xvjw05952w3z2mms12k27xd0y5fvqp9aar8kdl90la16ffjc2y9";
  };

  postUnpack = "sourceRoot+=/core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    ansi-terminal async base clock containers mtl optparse-applicative
    stm tagged unbounded-delays unix wcwidth
  ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "Modern and extensible testing framework";
  license = stdenv.lib.licenses.mit;
}
