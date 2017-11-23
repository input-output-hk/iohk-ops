#!/usr/bin/env bash

# ran to vote for the proposal

source config

for idx in {1..4}; do
  ./auxx/bin/cardano-auxx $COMMONOPTS $AUXXOPTS cmd --commands "vote ${idx} y ${PROPOSAL_ID}"
done
