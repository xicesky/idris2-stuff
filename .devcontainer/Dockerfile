FROM ghcr.io/xicesky/idris2-devcontainer:nightly-221220-ubuntu-22.04

RUN yes | unminimize
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install aptitude vim-nox tmux openjdk-11-jdk maven

COPY scripts/*.sh /tmp/scripts/
RUN /bin/bash /tmp/scripts/install-grml-zsh.sh
COPY bin/* /home/vscode/bin/
COPY .zshrc /home/vscode
RUN chown -R vscode: /home/vscode
