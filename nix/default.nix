args@{
  isProd      ? false
, isProfile   ? false
, isGhcjs     ? false
, isGhc86     ? false
}:
let release = import ./release.nix args;
    drvs = if isGhcjs then release.packagesGHCJS else release.thundermintPackages;
in # Merge all packages into single derivations to place in single result symlink
with release.pkgs.haskellPackages; release.pkgs.buildEnv {
  name = "thundermint";
  paths = release.pkgs.lib.attrValues drvs;
}
