args@{
  isProd      ? false
, isProfile   ? false
, isGhc86     ? false
, isGhcjs     ? false
}:
let release = import ./nix/release.nix args;
    pkgs = release.pkgs;
    drvs = if isGhcjs then release.packagesGHCJS else release.thundermintPackages;
in release.pkgs.haskellPackages.shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
    ];
    packages = _: pkgs.lib.attrValues drvs;
  }
