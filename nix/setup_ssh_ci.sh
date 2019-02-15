mkdir -p /etc/ssh
echo -n "$DRONE_GITHUB_PULL_KEY" > "/etc/ssh/id_rsa"
chmod 444 "/etc/ssh/id_rsa"
SSH_CONFIG=$(nix-build -E "let pkgs = import <nixpkgs> {}; in pkgs.writeText \"ssh-config\" (builtins.readFile ./nix/ssh-config-ci)")
echo $SSH_CONFIG
export NIX_PATH=ssh-config-file=$SSH_CONFIG:$NIX_PATH

# Setup SSH connection to cache server
mkdir -p $HOME/.ssh
echo -n "$NIX_CACHE_KEY" > "$HOME/.ssh/id_rsa"
chmod 700 "$HOME/.ssh/id_rsa"
echo "cache.hxr.team ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL3tegOekdPE722QGrDPhjAfEREMSwdA4oLDCp9Al499" >> "$HOME/.ssh/known_hosts"
