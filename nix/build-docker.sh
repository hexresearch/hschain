#!/usr/bin/env bash
set -e

# Configure cache
source ./setup_git.sh
export NIX_PATH=$GIT_NIX_PATH$NIX_PATH

containers=$(NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build containers.nix \
  --arg isProd true \
#  --arg containerTag \"develop\" \
  )

for container in $containers
do
  docker load < $container
done
