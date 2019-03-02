#!/usr/bin/env bash

source ./setup_git.sh

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

NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build --arg isProd true \
                                          --arg isGhcjs $isGhcjs \
                                          --arg isGhc86 $isGhc86
