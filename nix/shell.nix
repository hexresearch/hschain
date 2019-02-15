args@{
  isProd      ? false
, isProfile   ? false
, gitTag      ? null  # current tag
, buildNumber ? null  # CI build number
}:
let release = import ./release.nix args;
    pkgs = release.pkgs;
in release.pkgs.haskellPackages.shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
    ];
    buildInputs = with pkgs; [
      #rabbitmq_server
    ];
    packages = _: pkgs.lib.attrValues release.thundermintPackages;
    shellHook = ''
      ${pkgs.lib.optionalString (! builtins.isNull gitTag) "export GIT_TAG=${gitTag}"}
      ${pkgs.lib.optionalString (! builtins.isNull buildNumber) "export BUILD_NUMBER=${buildNumber}"}
    '';
  }
