{
  versions = ["ghc844" "ghc865" "ghcjs"];
  # List of packages with custom overrides
  derivations = {
    haskell = ./derivations/haskell;
    ghc844  = ./derivations/ghc84;
  };
  # Common overrides for librarise
  haskell = {
    katip-elasticsearch = { check = false; };
    Lazy-Pbkdf2         = { check = false; };
    serialise           = { check = false; };
    tasty               = { check = false; };
    cborg               = { check = false; };

    thundermint-crypto = { haddock = false; };
    thundermint-types  = { haddock = false; };
    thundermint        = { haddock = false; };
  };
  # Compiler specific overrides
  ghc865 = {
    bloodhound           = { check = false; jailbreak = true;};
    # https://github.com/tsuraan/bytestring-arbitrary/issues/10
    bytestring-arbitrary = { jailbreak = true; };
  };
  ghcjs = {
    SHA                   = { check = false; };
    aeson                 = { check = false; };
    half                  = { check = false; };
    quickcheck-assertions = { check = false; };
    scientific            = { check = false; };
    tasty-quickcheck      = { check = false; };
    tasty-hedgehog        = { check = false; };
    # Test have dependency on cryptonite
    Lazy-Pbkdf2           = { check = false; };
  };
}
