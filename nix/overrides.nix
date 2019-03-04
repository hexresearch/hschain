{
  versions = ["ghc844" "ghc863" "ghcjs"];
  # Common overrides for libraries
  haskell = {
    katip-elasticsearch = { check = false; };
    serialise           = { check = false; };
    tasty               = { check = false; };
    cborg               = { check = false; };

    thundermint-crypto = { haddock = false; };
    thundermint-types  = { haddock = false; };
    thundermint        = { haddock = false; };
  };
  # Compiler specific overrides
  ghc863 = {
    bloodhound           = { check = false; jailbreak = true;};
    bytestring-arbitrary = { jailbreak = true; };
  };
  ghcjs = {
    SHA                   = { check = false; };
    aeson                 = { check = false; };
    half                  = { check = false; };
    quickcheck-assertions = { check = false; };
    scientific            = { check = false; };
    tasty-quickcheck      = { check = false; };
  };
}
