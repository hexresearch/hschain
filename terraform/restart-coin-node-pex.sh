#!/bin/bash -e

# This script totally kill Terraform setup for coin-node-pex
# And then restart it

docker rm `docker ps -a --filter "name=node" -q` --force || true
docker image rm thundermint-node --force || true
docker container stop `docker container ls -a --filter "name=node" -q` || true
docker container rm `docker container ls -a --filter "name=node" -q` --force || true
docker volume rm thundermint --force || true
nix-build release.nix -A docker-container --max-jobs 9
docker load < result
terraform init terraform/coin-node-pex/
terraform apply -auto-approve terraform/coin-node-pex/
