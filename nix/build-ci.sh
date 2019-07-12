#!/usr/bin/env bash

set -e
shopt -s extglob

# Configure cache
./setup-cache.sh

# Minimal nsswitch.conf
[[ ! -e /etc/nsswitch.conf ]] && echo 'hosts: files dns' > /etc/nsswitch.conf

function push_nix_cache {
    # Sign nix output
    nix sign-paths -k /root/nix-cache-key.sec --all
    # Upload to nix cache
    echo /nix/store/!(*@(.drv|.lock)) | xargs nix-copy-closure -v --gzip --include-outputs --to root@cache.hxr.team
}

trap push_nix_cache EXIT

# Build backend packages
NIX_PATH=$GIT_NIX_PATH$NIX_PATH backend_output=$(nix-build --arg isProd true "$@")
# Get runtime deps
backend_deps=$(nix-store --query --requisites $backend_output)
