{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, th-lift, time, transformers, transformers-base, unix
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "0.5.3";
  sha256 = "a4739630d3dec1762e69b371e11d1caaa5e6a50aa13abe41dd0e34b3a7dd68ef";
  revision = "1";
  editedCabalFile = "0hhzkl88xk9j62897y11f4xx84qdh0mdap55iw30cl4zwlgp10ir";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text th-lift time transformers transformers-base
    unix wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Hedgehog will eat all your bugs";
  license = stdenv.lib.licenses.bsd3;
}
