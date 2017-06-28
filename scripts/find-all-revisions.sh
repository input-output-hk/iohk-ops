#!/bin/sh
find . -type f '!' -path '*/.git/*' '!' -path ./pkgs/default.nix -print0 | xargs -0 egrep --color '[^a-z0-9][a-z0-9]{40}[^a-z0-9]'

