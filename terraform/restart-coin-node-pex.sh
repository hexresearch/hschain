#!/bin/bash -e

# This script totally kill Terraform setup for coin-node-pex
# And then restart it

docker rm `docker ps -a --filter "name=node" -q` --force || true
docker image rm thundermint-node --force || true
docker image rm "localhost:5000/thundermint-node" --force || true
docker container stop `docker container ls -a --filter "name=node" -q` || true
docker container rm `docker container ls -a --filter "name=node" -q` --force || true
docker volume rm thundermint --force || true
nix-build release.nix -A docker-container --max-jobs 9
docker load < result
docker image tag thundermint-node localhost:5000/thundermint-node
docker push localhost:5000/thundermint-node
terraform init terraform/coin-node-pex/
yes "yes" | terraform apply terraform/coin-node-pex/
