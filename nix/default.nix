args@{
  isProd      ? false
, isProfile   ? false
, gitTag      ? null  # current tag
, buildNumber ? null  # CI build number
}:
let release = import ./release.nix args;
in # Merge all packages into single derivations to place in single result symlink
with release.pkgs.haskellPackages; release.pkgs.buildEnv {
  name = "thundermint";
  paths = release.pkgs.lib.attrValues release.thundermintPackages;
}
