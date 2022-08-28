#!/bin/bash

invoke_in_dir() {
    declare dir="$1"; shift
    (
        echo "$(printf "%q " cd "$dir")"
        cd "$dir"
        echo "$(printf "%q " "$@")"
        "$@"
    )
}

declare -a WORKSPACES=()
readarray -d '' WORKSPACES < <(
    find /workspaces -maxdepth 1 -type d -print0
) || return 1

for workspace in "${WORKSPACES[@]}" ; do
    [[ -x "$workspace/dependencies/build-and-install.sh" ]] || continue
    invoke_in_dir "$workspace/dependencies" "./build-and-install.sh"
done
