#!/bin/bash

declare -a DEPENDENCY_IPKGS=()

git-dependency() {
    declare dir="$1"; shift
    declare gituri="$1"; shift
    declare branch="$1"; shift
    declare ipkg="$1"; shift

    DEPENDENCY_IPKGS+=( "$dir/$ipkg" )
}

source dependencies.source.sh

# find-ipkg() {
#     declare dir="$1"; shift
#     readarray -d '' ipkgs < <(
#         find "$dir" -maxdepth 1 -iname "*.ipkg" -print0
#     ) || return 1
#     case ${#ipkgs[@]} in
#         0)  return 1; ;;    # not found
#         1)  echo "${ipkgs[0]}"; ;;  # found
#         *)  return 1; ;;    # more than one
#     esac
# }

# Build and install
declare -a FAILED_IPKGS=()
for ipkg in "${DEPENDENCY_IPKGS[@]}" ; do
    printf "%-38s: %s\n" "Building" "$ipkg"
    idris2 --build "$ipkg"; rc=$?
    if [[ "$rc" -ne 0 ]]; then
        FAILED_IPKGS+=( "$ipkg" )
    else
        printf "%-38s: %s\n" "Installing (sudo)" "$ipkg"
        sudo idris2 --install "$ipkg"; rc=$?
        if [[ "$rc" -ne 0 ]]; then
            FAILED_IPKGS+=( "$ipkg" )
        fi
    fi
done

echo "-------------------------------------------------------------------------------"
if [[ "${#FAILED_IPKGS[@]}" -gt 0 ]]; then
    echo "Some packages failed to build:"
    for i in "${FAILED_IPKGS[@]}" ; do
        echo " * $i"
    done
    exit 1
fi
