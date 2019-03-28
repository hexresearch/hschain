#!/usr/bin/env bash
set -xe

# Configure cache
./setup-cache.sh

# Minimal nsswitch.conf
[[ ! -e /etc/nsswitch.conf ]] && echo 'hosts: files dns' > /etc/nsswitch.conf

# Debug version variables
echo $DRONE_COMMIT
echo $DRONE_BRANCH
echo $DRONE_BUILD_NUMBER

# Build backend packages
NIX_PATH=$GIT_NIX_PATH$NIX_PATH backend_output=$(nix-build --arg isProd true "$@")
# Get runtime deps
backend_deps=$(nix-store --query --requisites $backend_output)

# Sign output
nix sign-paths -k /root/nix-cache-key.sec --all

# Upload to nix cache
shopt -s extglob
echo /nix/store/!(*@(.drv|.lock)) | xargs nix-copy-closure -v --gzip --include-outputs --to root@cache.hxr.team
