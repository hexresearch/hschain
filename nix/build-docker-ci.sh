#!/usr/bin/env bash
set -e

# Configure cache
./setup-cache.sh
export NIX_PATH=$GIT_NIX_PATH$NIX_PATH

branch=$DRONE_BRANCH
gittag=$DRONE_TAG
event=$DRONE_BUILD_EVENT
publish=false

# Minimal nsswitch.conf
[[ ! -e /etc/nsswitch.conf ]] && echo 'hosts: files dns' > /etc/nsswitch.conf

if [[ -n "${gittag// }" ]]; then
    echo "Publish with $gittag."
    tag=${gittag// }
    publish=true
else
  if [[ "$branch" == "master" && "$event" != "pull_request" ]]; then
  echo "Publish with latest"
  tag="latest"
  publish=true
  else
    echo "Not publish."
    tag="latest"
  fi
fi
# Decide whether to build profile
gittag=$DRONE_TAG
isProfile="false"
if [[ $gittag =~ "profile" ]]; then
  echo "It is profile build"
  isProfile="true"
fi
if [[ ! -z $gittag ]]; then
  gitTagArg="--arg gitTag \"\\\"$gittag\\\"\""
fi

containers=$(nix-build containers.nix --arg isProd true \
  --arg isProfile $isProfile \
  --arg containerTag \"$tag\" \
  --arg prefixName \"registry.hxr.team/\" \
  $gitTagArg \
  --arg buildNumber $DRONE_BUILD_NUMBER )

for container in $containers
do
  docker load < $container
done

if $publish; then
docker login --password $DOCKER_PASSWORD --username $DOCKER_USERNAME registry.hxr.team
docker push registry.hxr.team/thundermint-node:$tag
fi
