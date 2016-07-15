#!/usr/bin/env bash
set -e
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
mkdir -p etc/docker/_artifacts
stack install --local-bin-path=etc/docker/_artifacts "$@"
cp -r config static themes etc/docker/_artifacts/
