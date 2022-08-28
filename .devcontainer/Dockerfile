FROM ghcr.io/xicesky/idris2-devcontainer:v0.5.1-ubuntu-20.04

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install aptitude tmux rlwrap

COPY scripts/*.sh /tmp/scripts/
RUN /bin/bash /tmp/scripts/install-grml-zsh.sh

USER vscode
COPY bin/*.sh /home/vscode/bin/
COPY .zshrc /home/vscode
USER root
