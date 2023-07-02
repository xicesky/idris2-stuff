FROM ghcr.io/xicesky/idris2-devcontainer:nightly-230701-ubuntu-22.04

RUN yes | unminimize
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install aptitude vim-nox tmux python3-pip openjdk-11-jdk maven
RUN pip install pre-commit

COPY scripts/*.sh /tmp/scripts/
RUN /bin/bash /tmp/scripts/install-grml-zsh.sh
COPY bin/* /home/vscode/bin/
COPY .zshrc /home/vscode
RUN chown -R vscode: /home/vscode
