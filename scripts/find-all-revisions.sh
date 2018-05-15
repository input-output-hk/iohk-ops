#!/bin/sh
git ls-files | grep -v pkgs/default.nix | xargs grep -EH --color '[^a-z0-9][a-z0-9]{40}[^a-z0-9]'
exit 0
