# HOWTO download:
# $ docker pull scturtle/hathverse:alpine
FROM mitchty/alpine-ghc:latest
RUN apk update
RUN apk add curl musl-dev
RUN cabal update
RUN curl https://www.stackage.org/lts-5.11/cabal.config?global=true >> ~/.cabal/config
RUN cabal install hspec IOSpec split text unordered-containers vector
# build with:
# $ docker build -t scturtle/hathverse:alpine .
