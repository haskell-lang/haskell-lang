# Dockerfile for fpco/haskell-lang-base
#-*- mode: conf; -*-

# ubuntu:14.04
FROM ubuntu@sha256:d67ef8e385f1c8b13d8c3e7622dc31b51d07e5623c1d034ebe2acb14a11fb30d
ENV LANG=C.UTF-8
RUN apt-get update \
 && apt-get install -y --no-install-recommends libgmp10 ca-certificates libicu-dev \
 && apt-get clean
COPY _artifacts/themes/ /haskell-lang/themes/
COPY _artifacts/config/ /haskell-lang/config/
COPY _artifacts/haskell-lang /usr/local/bin/haskell-lang
COPY _artifacts/static/ /haskell-lang/static/
