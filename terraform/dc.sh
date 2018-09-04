#!/bin/bash

name=$1

# remove containers by name
# usage: ./dc.sh tls-node
docker rm $(docker ps -a --filter "name="${name} -q)
