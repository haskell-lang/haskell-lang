#!/usr/bin/env bash
set -ex
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
mkdir -p etc/docker/_artifacts
stack install --local-bin-path=etc/docker/_artifacts "$@"
rsync -av --delete config static themes etc/docker/_artifacts/
