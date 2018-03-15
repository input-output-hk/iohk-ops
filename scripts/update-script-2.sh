#!/usr/bin/env bash

# ran to propose an update to the cluster

# shellcheck source=scripts/config
source config

./auxx/bin/cardano-auxx "${COMMONOPTS[@]}" "${AUXXOPTS[@]}" cmd --commands "propose-update ${VOTER_INDEX} vote-all:true ${lastKnownBlockVersion} ~software~csl-daedalus:${applicationVersion} (upd-bin \"win64\" ${WIN64_INSTALLER}) (upd-bin \"macos64\" ${DARWIN_INSTALLER})"

echo you must now update the config file to contain the proposal ID
