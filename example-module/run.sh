#!/bin/bash

clear
ipkg="$(find . -maxdepth 1 -iname "*.ipkg" -printf '%f\n' | head -n 1)"
#pkgname="$(basename "$(pwd)")"
pkgname="$(echo "$ipkg" | grep -oP '^.*(?=\.ipk)')"
idris2 --build "$ipkg" || exit $?
if [[ -x "target/$pkgname" ]] ; then
    "target/$pkgname"
else
    echo "No executable named target/$pkgname"
fi
