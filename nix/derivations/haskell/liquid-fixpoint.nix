{ mkDerivation, ansi-terminal, array, ascii-progress, async
, attoparsec, base, binary, boxes, cereal, cmdargs, containers
, deepseq, directory, fetchgit, fgl, filepath, ghc-prim, git
, hashable, intern, mtl, nettools, ocaml, parallel, parsec, pretty
, process, stdenv, stm, syb, tasty, tasty-ant-xml, tasty-hunit
, tasty-rerun, text, text-format, transformers
, unordered-containers, z3
}:
mkDerivation {
  pname = "liquid-fixpoint";
  version = "0.8.0.2";
  src = fetchgit {
    url = "https://github.com/ucsd-progsys/liquid-fixpoint.git";
    sha256 = "07xwpczix4w3vhmzz5q8zbsawg7psy58mjr8zyqp14kp7fin3ykc";
    rev = "c47a4df6c02193882b8881ed968a13fa6d69b993";
    fetchSubmodules = true;
  };
  configureFlags = [ "-fbuild-external" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal array ascii-progress async attoparsec base binary
    boxes cereal cmdargs containers deepseq directory fgl filepath
    ghc-prim hashable intern mtl parallel parsec pretty process syb
    text text-format transformers unordered-containers
  ];
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ ocaml ];
  testHaskellDepends = [
    base containers directory filepath mtl process stm tasty
    tasty-ant-xml tasty-hunit tasty-rerun transformers
  ];
  testSystemDepends = [ git nettools z3 ];
  doCheck = false;
  homepage = "https://github.com/ucsd-progsys/liquid-fixpoint";
  description = "Predicate Abstraction-based Horn-Clause/Implication Constraint Solver";
  license = stdenv.lib.licenses.bsd3;
}
