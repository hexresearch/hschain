{ mkDerivation, base, bifunctors, comonad, containers, distributive
, exceptions, mtl, profunctors, semigroupoids, semigroups, stdenv
, template-haskell, transformers, transformers-base
, transformers-compat
}:
mkDerivation {
  pname = "free";
  version = "5.0.2";
  sha256 = "ef05eb1c8e69742a4f962c573ef362e44ad48772940f1ef69fe39f0f77b2a396";
  libraryHaskellDepends = [
    base bifunctors comonad containers distributive exceptions mtl
    profunctors semigroupoids semigroups template-haskell transformers
    transformers-base transformers-compat
  ];
  homepage = "http://github.com/ekmett/free/";
  description = "Monads for free";
  license = stdenv.lib.licenses.bsd3;
}
