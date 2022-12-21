#!/bin/bash

ipkg="$(find . -maxdepth 1 -iname "*.ipkg" -printf '%f\n' | head -n 1)"
pkgname="$(echo "$ipkg" | grep -oP '^.*(?=\.ipk)')"
pack install "$pkgname"
