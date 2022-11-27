#!/bin/bash

git-dependency() {
    declare dir="$1"; shift
    declare gituri="$1"; shift
    declare branch="$1"; shift

    # Clone or switch branch
    if [ -d "$dir" ] ; then (
        cd "$dir"
        #git pull
        git checkout "$branch" || return $?
    ) else
        git clone -b "$branch" "$gituri" "$dir" || return $?
    fi
}

source dependencies.source.sh
