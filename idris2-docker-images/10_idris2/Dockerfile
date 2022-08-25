# syntax = docker/dockerfile:1.4
FROM xicesky/idris2-base AS build

COPY . /opt/idris2
WORKDIR /opt/idris2

# Clone idris2 repo if it's not present yet
RUN <<EOF
    if [ ! -d idris2-src ] ; then
        git clone "https://github.com/idris-lang/Idris2.git" idris2-src
    fi
EOF

WORKDIR /opt/idris2/idris2-src

ENV SCHEME=scheme

RUN make bootstrap

RUN make install
ENV PATH="/root/.idris2/bin:${PATH}"
RUN make clean
RUN make all && make install && make install-api

# FROM ubuntu:20.04

# RUN apt-get update && apt-get install --yes make chezscheme && rm -rf /var/lib/apt/lists/*

# RUN mkdir -p /root/.idris2

# COPY --from=build /root/.idris2 /root/.idris2

# ENV PATH="/root/.idris2/bin:${PATH}"
