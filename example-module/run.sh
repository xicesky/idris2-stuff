#!/bin/bash

clear
ipkg="$(find . -maxdepth 1 -iname "*.ipkg" -printf '%f\n' | head -n 1)"
pack run "$ipkg"

# Note: Alternatively, you can run the installed package using
# pkgname="$(echo "$ipkg" | grep -oP '^.*(?=\.ipk)')"
# pack install "$pkgname"
