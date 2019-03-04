# Interpreter for specification for flags for haskell packages
pkgs:
specification:
# Common definitions
let
  foldl'  = builtins.foldl';
  hasAttr = builtins.hasAttr;
  id      = x: x;
  lib     = pkgs.haskell.lib;
in let
  # Merge specifications for each GHC version
  merge = dct: ghc: dct // {
    "${ghc}" = specification.haskell // (specification."${ghc}" or {});
  };
  mergedSpec = foldl' merge {} specification.versions;
in let
  # Modify derivation according to specification
  modifyDerivation = spec: deriv:
    let
      evalCheck =
        if hasAttr "check" spec
        then (if spec.check then lib.doCheck else lib.dontCheck)
        else id;
      evalHaddock =
        if hasAttr "haddock" spec
        then (if spec.haddock then lib.doHaddock else lib.dontHaddock)
        else id;
      evalJailbreak =
        if hasAttr "jailbreak" spec
        then (if spec.jailbreak then lib.doJailbreak else lib.dontJailbreak)
        else id;
    in
      evalJailbreak (evalHaddock (evalCheck deriv));
  # Modify packages
  modifyPackage = spec: super: name:
    super // { "${name}" = modifyDerivation spec."${name}" super."${name}"; }
  ;
  #
  modifyGhcVersion = _: spec: super:
    foldl' (modifyPackage spec) super (builtins.attrNames spec)
  ;
in
  builtins.mapAttrs modifyGhcVersion mergedSpec
