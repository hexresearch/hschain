{
  # List of GHC versions for which we apply overrides
  versions = ["ghc844" "ghc865" "ghc883" "ghc8101" "ghcjs"];
  # List of packages with custom overrides
  derivations = {
    haskell = ./derivations/haskell;
    ghc844  = ./derivations/ghc84;
    ghc865  = ./derivations/ghc86;
    ghc883  = ./derivations/ghc88;
    ghc8101 = ./derivations/ghc810;
  };
  # Common overrides for librarise
  haskell = {
    # https://github.com/tsuraan/bytestring-arbitrary/issues/10
    bytestring-arbitrary = { jailbreak = true; };
    Diff                 = { check = false; };
    #
    hschain-crypto = { haddock = false; };
    hschain-types  = { haddock = false; };
    hschain        = { haddock = false; };
  };
  # Compiler specific overrides
  ghc865 = {
    servant-server = { check = false; };
  };
  ghc883 = {
  };
  ghc8101 = {
    generic-lens    = { check     = false; };
    monad-par       = { check     = false; };
    http-media      = { jailbreak = true; };
    cborg           = { jailbreak = true; };
    serialise       = { jailbreak = true; };
    microlens-th    = { jailbreak = true; };
    retry           = { jailbreak = true; };
    safe-exceptions = { jailbreak = true; };
    vault           = { jailbreak = true; };
    servant         = { jailbreak = true; check = false; };
    servant-server  = { jailbreak = true; check = false; };
  };
  ghcjs = {
    SHA                   = { check = false; };
    cborg                 = { check = false; };
    half                  = { check = false; };
    quickcheck-assertions = { check = false; };
    scientific            = { check = false; };
    tasty-quickcheck      = { check = false; };
    QuickCheck            = { check = false; };
    lens                  = { check = false; };
    generic-lens          = { check = false; };
    comonad               = { check = false; };
    semigroupoids         = { check = false; };
    # Test have dependency on cryptonite
    Lazy-Pbkdf2           = { check = false; };
  };
}
