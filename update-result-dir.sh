#!/usr/bin/env bash

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$dir"

new_cardano_sl=`stat -c '%X %n' /nix/store/*-cardano-sl-0.1.0.0 | sort| tail -1 | sed 's/\S*\s//'`

rm result
ln -s $new_cardano_sl result
