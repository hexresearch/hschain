#!/usr/bin/env bash
set -e

# Configure cache
./setup-cache.sh
export NIX_PATH=$GIT_NIX_PATH$NIX_PATH

# Minimal nsswitch.conf
[[ ! -e /etc/nsswitch.conf ]] && echo 'hosts: files dns' > /etc/nsswitch.conf

gitbranch=$DRONE_BRANCH
gittag=$DRONE_TAG
gitevent=$DRONE_BUILD_EVENT

publish="false"

# Decide what artifcats to build and publish
if [[ -n "${gittag// }" ]]; then
    tag=${gittag// }
    publish="true"
    gitTagArg="--arg gitTag \"\\\"$gittag\\\"\""
    echo "Publish with $gittag"
else
  if [[ "$gitbranch" == "master" && "$gitevent" != "pull_request" ]]; then
  tag="latest"
  publish="true"
  echo "Publish with latest"
  else
    tag="latest"
    echo "Not publish"
  fi
fi

# Debug drone variables
echo "git branch: $gitbranch"
echo "git tag: $gittag"
echo "git event: $gitevent"

containers=$(nix-build containers.nix --arg isProd true \
  --arg containerTag \"$tag\" \
  --arg prefixName \"registry.hxr.team/\" )

for container in $containers
do
  docker load < $container
done

if $publish; then
docker login --password $DOCKER_PASSWORD --username $DOCKER_USERNAME registry.hxr.team
docker push registry.hxr.team/thundermint-node:$tag
fi
