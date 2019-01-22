#!/usr/bin/env sh
set -e

REGISTRY=registry.hxr.team

branch=$DRONE_BRANCH
gittag=$DRONE_TAG
event=$DRONE_BUILD_EVENT
publish=false

if [[ -n "${gittag// }" ]]; then
    tag=${gittag// }
    publish=true
else
    if [[ "$branch" == "master" && "$event" != "pull_request" ]]; then
        tag="latest"
        publish=true
    else
        tag="latest"
    fi
fi

docker build -t $REGISTRY/thundermint:$tag .

if $publish; then
    docker login --password $DOCKER_PASSWORD --username $DOCKER_USERNAME $REGISTRY
    docker push $REGISTRY/thundermint:$tag
fi
