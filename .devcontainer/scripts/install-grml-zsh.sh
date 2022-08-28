#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

has_apt_lists() {
    [[ -d "/var/lib/apt/lists" ]] || return 1
    find /var/lib/apt/lists/ -maxdepth 1 -type f 2>/dev/null | grep -q .
}

ensure_apt_get_update() {
    if ! has_apt_lists; then
        apt-get update
    fi
}

# Install zsh and required utilities. If zsh exists, assume we already have everything.
if ! type zsh > /dev/null 2>&1; then
    ensure_apt_get_update || exit 1
    apt-get install -y git zsh coreutils grep sed procps txt2tags build-essential || exit 1
fi

cd /usr/local/src || exit 1
git clone "https://github.com/grml/grml-etc-core.git" "grml-etc-core" || exit 1
cd grml-etc-core || exit 1
install -D -m644 etc/skel/.zshrc "/etc/skel/.zshrc" || exit 1
install -D -m644 etc/zsh/keephack "/etc/zsh/keephack" || exit 1
install -D -m644 etc/zsh/zshrc "/etc/zsh/zshrc" || exit 1
