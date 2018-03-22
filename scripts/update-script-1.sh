#!/usr/bin/env bash

# ran to initialize the secret keys and auxx/tools symlinks

# edit these to fit the update
# shellcheck source=scripts/config
source config

nix-build .. -A cardano-sl-auxx -o auxx
nix-build .. -A cardano-sl-tools -o tools
for idx in {0..6}; do
  ./tools/bin/cardano-keygen "${COMMONOPTS[@]}" rearrange --mask "key${idx}.sk"
done
./auxx/bin/cardano-auxx "${COMMONOPTS[@]}" "${AUXXOPTS[@]}" cmd --commands "add-key ./key0.sk primary:true; add-key ./key1.sk primary:true; add-key ./key2.sk primary:true; add-key ./key3.sk primary:true; add-key ./key4.sk primary:true; add-key ./key5.sk primary:true; add-key ./key6.sk primary:true; listaddr"
