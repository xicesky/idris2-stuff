#!/bin/sh

#./build.sh || exit $?

source ./image-config.sh

DOCKER_IMAGE_TAG="latest"
DOCKER_IMAGE="$DOCKER_IMAGE_NAME:$DOCKER_IMAGE_TAG"

docker run --rm -it \
    -v "$(pwd)":/mnt -w /mnt \
    "$DOCKER_IMAGE" \
    bash \
    || exit $?
