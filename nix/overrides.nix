{
  versions = ["ghc844" "ghc865" "ghcjs"];
  # List of packages with custom overrides
  derivations = {
    haskell = ./derivations/haskell;
    ghc844  = ./derivations/ghc84;
  };
  # Common overrides for librarise
  haskell = {
    Diff                = { check = false; };
    #
    hschain-crypto = { haddock = false; };
    hschain-types  = { haddock = false; };
    hschain        = { haddock = false; };
  };
  # Compiler specific overrides
  ghc865 = {
    # https://github.com/tsuraan/bytestring-arbitrary/issues/10
    bytestring-arbitrary = { jailbreak = true; };
  };
  ghcjs = {
    SHA                   = { check = false; };
    cborg                 = { check = false; };
    half                  = { check = false; };
    quickcheck-assertions = { check = false; };
    scientific            = { check = false; };
    tasty-quickcheck      = { check = false; };
    QuickCheck            = { check = false; };
    # Test have dependency on cryptonite
    Lazy-Pbkdf2           = { check = false; };
  };
}
