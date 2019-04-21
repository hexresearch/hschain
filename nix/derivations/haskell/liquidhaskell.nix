{ mkDerivation, aeson, base, binary, bytestring, cereal, cmdargs
, containers, data-default, deepseq, Diff, directory, fetchgit
, filepath, fingertree, ghc, ghc-boot, ghc-paths, ghc-prim, githash
, gitrev, hashable, hscolour, liquid-fixpoint, mtl
, optparse-applicative, optparse-simple, parsec, pretty, process
, stdenv, stm, syb, tagged, tasty, tasty-ant-xml, tasty-hunit
, tasty-rerun, template-haskell, temporary, text, time
, transformers, unordered-containers, vector, z3
}:
mkDerivation {
  pname = "liquidhaskell";
  version = "0.8.6.0";
  src = fetchgit {
    url = "https://github.com/ucsd-progsys/liquidhaskell.git";
    sha256 = "02r4hxzjk96765flx5la0bk803rm5ypxi54cgf6xisqikx68h5kd";
    rev = "e725abded15fc8f7cbedd66781ad56345318f6b4";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base binary bytestring cereal cmdargs containers data-default
    deepseq Diff directory filepath fingertree ghc ghc-boot ghc-paths
    ghc-prim githash gitrev hashable hscolour liquid-fixpoint mtl
    optparse-simple parsec pretty syb template-haskell temporary text
    time transformers unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers directory filepath liquid-fixpoint mtl
    optparse-applicative parsec process stm syb tagged tasty
    tasty-ant-xml tasty-hunit tasty-rerun text transformers
  ];
  testSystemDepends = [ z3 ];
  homepage = "https://github.com/ucsd-progsys/liquidhaskell";
  description = "Liquid Types for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
