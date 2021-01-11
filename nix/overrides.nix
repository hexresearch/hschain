{
  # List of GHC versions for which we apply overrides
  versions = ["ghc844" "ghc865" "ghc883" "ghc884" "ghc8102" "ghcjs"];
  # List of packages with custom overrides
  derivations = {
    haskell = ./derivations/haskell;
    ghc844  = ./derivations/ghc84;
    ghc865  = ./derivations/ghc86;
    ghc883  = ./derivations/ghc88;
    ghc884  = ./derivations/ghc88;
    ghc8102 = ./derivations/ghc810;
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
  ghc884 = {
  };
  ghc8102 = {
    http-media      = { jailbreak = true; };
    servant         = { jailbreak = true; check = false; };
    servant-server  = { jailbreak = true; check = false; };
  };
  ghcjs = {
    SHA                   = { check = false; };  # test hangs
    tasty-quickcheck      = { check = false; };  # test hangs
    scientific            = { check = false; };  # test hangs
    QuickCheck            = { check = false; };  # test hangs
    base-compat-batteries = { check = false; };  # test fails
    time-compat           = { check = false; };  # test fails
    vector                = { check = false; };  # tests take infinity
    # --
    half                  = { check = false; };   # doctests
    quickcheck-assertions = { check = false; };   # doctests
    lens                  = { check = false; };   # doctests
    generic-lens          = { check = false; };   # inspection-tests
    comonad               = { check = false; };   # doctests
    temporary             = { check = false; };   # doctests
    semigroupoids         = { check = false; };   # doctests
    # Test have dependency on cryptonite
    Lazy-Pbkdf2           = { check = false; };
  };
}
