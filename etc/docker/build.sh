#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
docker build -t fpco/haskell-lang-base etc/docker/haskell-lang-base
stack image container "$@"
