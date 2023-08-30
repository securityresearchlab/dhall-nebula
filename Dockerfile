FROM haskell:9.0.2 as builder
WORKDIR /home/app
ADD ./tool /home/app/tool
ADD ./dhall /home/app/dhall
WORKDIR /home/app/tool
RUN stack build && stack install && ls /root/.local/bin

FROM debian:stretch-slim AS downloads
WORKDIR /home/app
RUN apt-get update && apt-get install curl bzip2 -y
RUN curl -L https://github.com/dhall-lang/dhall-haskell/releases/download/1.41.1/dhall-yaml-1.2.10-x86_64-linux.tar.bz2 --output /home/app/dhall.tar.bz2
RUN tar --extract --bzip2 --file /home/app/dhall.tar.bz2
RUN curl -L https://github.com/slackhq/nebula/releases/download/v1.6.0/nebula-linux-amd64.tar.gz --output nebula.tar.gz
RUN tar -xvf nebula.tar.gz

FROM ubuntu:20.04 AS tool
COPY --from=builder /root/.local/bin/tool /bin/
COPY --from=downloads /home/app/bin/dhall-to-yaml-ng /usr/local/bin
COPY --from=downloads /home/app/nebula /usr/local/bin
COPY --from=downloads /home/app/nebula-cert /usr/local/bin
WORKDIR /home/app/dhall
ADD ./dhall /home/app/dhall
