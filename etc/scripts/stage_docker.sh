#!/usr/bin/env bash
set -e
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
stack install --local-bin-path=etc/docker "$@"
cp -r config static themes etc/docker/
