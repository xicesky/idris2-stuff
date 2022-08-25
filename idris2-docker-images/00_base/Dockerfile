# syntax = docker/dockerfile:1.4
#FROM snazzybucket/idris2api:v0.5.1-2-ga0846af5
FROM ubuntu:20.04 AS build

ARG DEBIAN_FRONTEND=noninteractive

# Add apt-cacher-ng
RUN <<EOF
    echo 'Acquire::HTTP::Proxy "http://172.17.0.1:3142";' >> /etc/apt/apt.conf.d/01proxy && \
    echo 'Acquire::HTTPS::Proxy "false";' >> /etc/apt/apt.conf.d/01proxy
EOF
RUN apt-get clean && rm -rf /var/lib/apt/lists/* && apt-get update

# Build requirements
RUN apt-get install --yes git gcc make chezscheme libgmp3-dev

# Java requirements: jdk & maven
RUN apt-get install --yes openjdk-11-jdk maven

# Cleanup
RUN rm -rf /var/lib/apt/lists/*
