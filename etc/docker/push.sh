#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
exec "$(dirname "${BASH_SOURCE[0]}")/../common/devops-helpers/docker/push_helper.sh" \
    --repo fpco/haskell-lang "$@"
