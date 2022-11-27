FROM ghcr.io/xicesky/idris2-devcontainer:v0.6.0-ubuntu-22.04

RUN yes | unminimize
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install aptitude vim-nox tmux rlwrap

COPY scripts/*.sh /tmp/scripts/
RUN /bin/bash /tmp/scripts/install-grml-zsh.sh

USER vscode
COPY bin/*.sh /home/vscode/bin/
COPY .zshrc /home/vscode
USER root
