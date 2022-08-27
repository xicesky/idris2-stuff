#!/bin/bash

(
    cd idris2-elab-util
    idris2 --build elab-util.ipkg
    sudo idris2 --install elab-util.ipkg
)

(
    cd idris2-sop
    idris2 --build sop.ipkg
    sudo idris2 --install sop.ipkg
)
