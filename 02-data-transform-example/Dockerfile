FROM amazonlinux:latest

# haskell-platform installs ghc and cabal
RUN yum update -y && \
    yum install -y make epel-release && \
    yum install -y haskell-platform --enablerepo=epel

RUN cabal update && cabal install cabal-install

WORKDIR /serverless-haskell

COPY . /serverless-haskell

RUN make clean
RUN make build

# TODO: do something with the binary
