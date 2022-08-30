#!/bin/bash

clear
rlwrap -P ':module Repl' idris2 --repl java-support.ipkg # src/main/idris2/Repl.idr
