#!/usr/bin/env bash
set -xe

# Configure cache
./setup-cache.sh

# Minimal nsswitch.conf
[[ ! -e /etc/nsswitch.conf ]] && echo 'hosts: files dns' > /etc/nsswitch.conf

isGhcjs="false"
isGhc86="false"

while [ "$1" != "" ]; do
  case $1 in
    --enableGhcjs)  echo "Ghcjs build"
                    isGhcjs="true"
                    ;;
    --enableGhc86)  echo "Ghc86 build"
                    isGhc86="true"
                    ;;
    *)              echo "You can specify --enableGhcjs or --enableGhc86"
                    exit 1
  esac

  shift
done

# Debug version variables
echo $DRONE_COMMIT
echo $DRONE_BRANCH
echo $DRONE_BUILD_NUMBER

# Build backend packages
NIX_PATH=$GIT_NIX_PATH$NIX_PATH backend_output=$(nix-build --arg isProd true \
  --arg isGhc86 $isGhc86 \
  --arg isGHCJS $isGhcjs )
# Get runtime deps
backend_deps=$(nix-store --query --requisites $backend_output)

# Sign output
nix sign-paths -k /root/nix-cache-key.sec --all

# Upload to nix cache
#nix-copy-closure -v --gzip --include-outputs --to root@cache.hxr.team $front_res
shopt -s extglob
echo /nix/store/!(*@(.drv|.lock)) | xargs nix-copy-closure -v --gzip --include-outputs --to root@cache.hxr.team
