#!/bin/bash

clear
ipkg="$(find . -maxdepth 1 -iname "*.ipkg" -printf '%f\n' | head -n 1)"
pack --with-ipkg "$ipkg" repl src/main/idris2/Repl.idr
