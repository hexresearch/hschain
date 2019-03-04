# Declaratively (more or less) configure package set for the haskell
# development. This functions generates overrides for several packages
# at once and meanty to be used in following manner:
#
# > config = {
# >   ...
# >   packageOverrides = super: {
# >     haskell = import ./interpret pkgs super {
# >       flags         = {...};
# >       extraPackages = ./derivations;
# >       release       = customPackages;
# >     };
# >     ...
# >   };
# > };
# >
#
# Fields of parameter record have folloing meaning:
#
# * flags contains adjustments to flags of package such as
#   dontCheck/dontHaddock/haddock. They could be specified both for all
#   versions of GHC and on per-GHC basis.
#
# * extraPackages is directory containing nix expressions for packages
#   not in standard set.
#
# * release nix expressions for any other packages

pkgs:
super:
spec:
let
  lib = pkgs.haskell.lib;
in let
  # Packages which are not in the default set so we add them manually
  extraPackages = lib.packagesFromDirectory {
    directory = spec.extraPackages;
  };
  # Modifications to flags of packages
  flagOverrides = import ./flags.nix pkgs spec.flags;
in let
  # Create overrides for different GHC version
  makeOverride = super: ghc: addFlags:
    super."${ghc}".override {
      overrides = hsSelf: hsSuper:
        addFlags (hsSuper // extraPackages hsSelf hsSuper // spec.release hsSuper)
      ;
    };
  overridesPerGhc = super:
    (builtins.mapAttrs (makeOverride super) flagOverrides)
  ;
in
super.haskell // {
  packages = super.haskell.packages // overridesPerGhc super.haskell.packages;
}
