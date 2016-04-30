#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
exec "$(dirname ${BASH_SOURCE[0]})/../common/devops-helpers/kubernetes/deploy_rc_helper.sh" \
     --app "haskell-lang" \
     --repo fpco/haskell-lang \
     --specdir "$(dirname "${BASH_SOURCE[0]}")" \
     --clusters ~/.kube/clusters/fpco-prod-us-east-1?/kubeconfig \
     "$@"
