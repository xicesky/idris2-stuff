#!/bin/bash

# docker run --rm -it \
#     -v "$(pwd)":/mnt -w /mnt \
#     ghcr.io/xicesky/idris2:v0.5.1-ubuntu-20.04 \
#     "$@"

idris2 --build java-support.ipkg || exit $?
sudo idris2 --install java-support.ipkg || exit $?
