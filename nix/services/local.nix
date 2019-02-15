# NixOS модуль который можно включить в локальную NixOS для отладки системы
{ config, options, lib, pkgs, ... }:
with lib;
let
  cfg  = config.services.xenochain;
in {
  ###### dependencies
  imports = [
    ./bitcoin.nix
    ./local-secrets.nix
  ];

  ###### interface

  options = {

    services.xenochain = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Starts all services needed to perform integration tests for xenochain localy.
        '';
      };

    };
  };

  ###### implementation
  config = mkIf cfg.enable {
    services.bitcoin = {
      enable = true;
      testnet = true;
      nodePort = 8082;
    };
    deployment.keys = {
      rpcpassword.text = "AeNungaengohpeeG4eX5"; # Change to your own password
    };
  };
}
