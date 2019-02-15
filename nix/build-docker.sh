#!/usr/bin/env bash
set -e

# Configure cache
source ./setup_git.sh
export NIX_PATH=$GIT_NIX_PATH$NIX_PATH

tag=$(git tag -l --points-at HEAD)
if [ ! -z $tag ]; then
  GIT_TAG_ARG="--arg gitTag \"\\\"$tag\\\"\""
fi

containers=$(NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build containers.nix \
  --arg isProd true \
  --arg containerTag \"develop\" \
  $GIT_TAG_ARG \
  )

for container in $containers
do
  docker load < $container
done
