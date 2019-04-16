#!/bin/sh

nix-instantiate --eval check-updates.nix | sed 's/^"//; s/"$//; s/\\"/"/g'
