#!/bin/sh
#
# Start jupyter notebook server
set -e
nix-shell --command 'PYTHONPATH=$(pwd) jupyter notebook --no-browser' jupyter.nix
