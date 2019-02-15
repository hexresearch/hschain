#!/bin/bash
#
# USAGE: ./run-screen
#   Starts screen and daemonized ssh-agent. After that it's possible
#   to work with screen normally: detach, reattach etc.
#
#
#
# Script to allow development on NixOS node as root over ssh. Main
# problem is cloning of private repositories. To that end we need:
#
#  * Running ssh-agent. Normally it's started as part of X session bu
#    we don't have X here.
#
#  * Give access to ssh-agent's socket to nixbld users.


# Not most elegant cleanup. But will work in single user-setup
killall ssh-agent
killall socat
set -e

# We need to build suitable ssh_config which will be accepted by
# nix. That means in must be located in /nix/store
SSH_CFG=$HOME/xenochain-ssh-config
nix_expr=$(cat <<EOF
let
  pkgs = import <nixpkgs> {};
in
  pkgs.writeText "ssh_config"
    ''
    StrictHostKeyChecking=no
    UserKnownHostsFile /dev/null
    ''
EOF
)
nix-build --out-link $SSH_CFG --expr "$nix_expr"

# Start ssh agent and provide passphrase to it
eval $(ssh-agent)
ssh-add

# Start socat so that nix builder has access to ssh-agent
SOCK=$(mktemp -u -t nix-pipe.XXXXXX)
rm -f "$SOCK"
nohup socat UNIX-LISTEN:$SOCK,fork,mode=0070,group=nixbld UNIX-CLIENT:$SSH_AUTH_SOCK &> /dev/null &

# Start screen with NIX_PATH correctly set
export NIX_PATH=ssh-config-file=$(readlink $SSH_CFG):ssh-auth-sock=$SOCK:$NIX_PATH
screen
