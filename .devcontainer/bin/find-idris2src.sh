#!/bin/bash

find /usr/local/src/idris2/libs -iname "*.idr" \
    -exec grep -iPe "$1" {} +
