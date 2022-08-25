# syntax = docker/dockerfile:1.4
FROM xicesky/idris2-base AS build

COPY . /opt/idris2
WORKDIR /opt/idris2

# Clone idris2 repo if it's not present yet
RUN <<EOF
    if [ ! -d idris2-src ] ; then
        git clone "https://github.com/mmhelloworld/idris-jvm.git" idris2-src
    fi
EOF

WORKDIR /opt/idris2/idris2-src

ENV SCHEME=scheme

RUN make bootstrap

RUN make install
ENV PATH="/root/.idris2/bin:${PATH}"

RUN mvn clean package
