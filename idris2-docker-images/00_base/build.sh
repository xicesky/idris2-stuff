#!/bin/bash

source image-config.sh

DOCKER_IMAGE_TAG="latest"
DOCKER_IMAGE="$DOCKER_IMAGE_NAME:$DOCKER_IMAGE_TAG"

docker build . -t "$DOCKER_IMAGE" \
    || exit $?
