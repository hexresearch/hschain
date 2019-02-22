args@{
  isProd      ? false
, isProfile   ? false
, isGHCJS     ? false
}:
let release = import ./release.nix args;
    drvs = if isGHCJS then release.packagesGHCJS else release.thundermintPackages;
in # Merge all packages into single derivations to place in single result symlink
with release.pkgs.haskellPackages; release.pkgs.buildEnv {
  name = "thundermint";
  paths = release.pkgs.lib.attrValues drvs;
}
