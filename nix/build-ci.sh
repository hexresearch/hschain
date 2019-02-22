#!/usr/bin/env bash
set -xe

# Configure cache
./setup-cache.sh

# Minimal nsswitch.conf
[[ ! -e /etc/nsswitch.conf ]] && echo 'hosts: files dns' > /etc/nsswitch.conf

isGHCJS="false"
if [ "$1" == "GHCJS" ]; then
  echo "GHCJS build"
  isGHCJS="true"
fi

# Debug version variables
echo $DRONE_COMMIT
echo $DRONE_BRANCH
echo $DRONE_BUILD_NUMBER

# Build backend packages
NIX_PATH=$GIT_NIX_PATH$NIX_PATH backend_output=$(nix-build --arg isProd true \
  --arg isProfile $isProfile \
  --arg isGHCJS $isGHCJS \
  $gitTagArg \
  --arg buildNumber $DRONE_BUILD_NUMBER )
# Get runtime deps
backend_deps=$(nix-store --query --requisites $backend_output)

# Sign output
nix sign-paths -k /root/nix-cache-key.sec --all

# Upload to nix cache
#nix-copy-closure -v --gzip --include-outputs --to root@cache.hxr.team $front_res
shopt -s extglob
echo /nix/store/!(*@(.drv|.lock)) | xargs nix-copy-closure -v --gzip --include-outputs --to root@cache.hxr.team
