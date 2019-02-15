# NixOS module to pipe authorization from local user to nix daemon
{ config, options, lib, pkgs, ... }:
with lib;
let
  cfg  = config.services.private-ssh-pipe;
  socatScript = pkgs.writeScript "socat-pipe" ''
    if [ -e ${cfg.socket-path} ]; then
      rm ${cfg.socket-path}
    fi
    ${pkgs.socat}/bin/socat UNIX-LISTEN:${cfg.socket-path},fork,mode=0070,group=nixbld UNIX-CLIENT:$SSH_AUTH_SOCK
  '';
  sshConfigFile = pkgs.writeText "ssh-config" cfg.ssh-config;
in {

  ###### interface

  options = {

    services.private-ssh-pipe = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Starts system-wide socat that pipes local user SSH agent socket to nix daemon.
        '';
      };

      user = mkOption {
        type = types.str;
        description = ''
          Which local user SSH agent socket to use.
        '';
      };

      ssh-config = mkOption {
        type = types.str;
        default = ''
          StrictHostKeyChecking=no
          UserKnownHostsFile /dev/null
        '';
        description = ''
          Which ssh config to setup to nix daemon.
        '';
      };

      socket-path = mkOption {
        type = types.str;
        default = "/tmp/hax";
        description = ''
          Where to store proxy socket for piping user SSH agen socket.
        '';
      };

      user-socket = mkOption {
        type = types.str;
        default = "%t/keyring/ssh";
        description = ''
          Where user socket is located. You can use %t to expand to run dir like '/run/user/1000'
        '';
      };
    };
  };

  ###### implementation

  config = mkIf cfg.enable {
    systemd.user.services.ssh-agent-pipe = {
        enable = true;
        description = "Pipe authorization from local user to nix daemon";
        wantedBy = [ "default.target" ];
        environment = {
          SSH_AUTH_SOCK = cfg.user-socket;
        };
        serviceConfig = {
          ExecStart = let
            script = pkgs.writeScript "ssh-agent-pipe-run" ''
              export PATH="$(systemctl --user show-environment | ${pkgs.gnused}/bin/sed 's/^PATH=\(.*\)/\1/; t; d')"
              sudo ${socatScript}
            '';
            in "${pkgs.bash}/bin/bash ${script}";
          Restart = "always";
          RestartSec = 5;
        };
    };
    security.sudo.enable = true;
    security.sudo.extraConfig = ''
      ${cfg.user} ALL=(ALL) NOPASSWD: ${socatScript}
      '';
    nix.nixPath = options.nix.nixPath.default ++ # Comment if you already set it somewhere.
      [
        "ssh-auth-sock=${cfg.socket-path}"
        "ssh-config-file=${sshConfigFile}"
      ];
  };
}
