#!/bin/bash

clear
rlwrap -P ':module Repl' idris2 --repl "$(find . -maxdepth 1 -iname "*.ipkg" | head -n 1)"
