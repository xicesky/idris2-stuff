#!/bin/bash

ipkg="$(find . -maxdepth 1 -iname "*.ipkg" -printf '%f\n' | head -n 1)"
idris2 --build "$ipkg" || exit $?
sudo idris2 --install "$ipkg" || exit $?
