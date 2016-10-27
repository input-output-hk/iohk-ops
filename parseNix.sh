#!/bin/sh

# Gets a host by instance name
function getNixopsHost {
    nixops info | grep $1 | sed -e "s/.*|\s\([0-9\.]\+\)\s*|\s*/\1/"
}

function parseNixopsIp {
    cat nixops.nix | grep -E "$1\s*=\s*\"" | sed -e "s/.*\"\([0-9\.]\+\)\".*/\1/"
}

function parseNixopsPort {
    cat nixops.nix | grep -E "$1\s*=\s*[0-9]+" | sed -e "s/.*=\s*\([0-9]\+\);.*/\1/" 
}

NIXOPS_BANK_HOST=$(parseNixopsIp bankIp)
NIXOPS_BANK_PORT=$(parseNixopsPort bankPort)
#NIXOPS_NOTARY_HOST=$(parseNixopsPort notaryIp)
#NIXOPS_NOTARY_PORT=$(parseNixopsPort notaryPort)
NIXOPS_MINTETTE_PORT=$(parseNixopsPort mintettePort)
NIXOPS_EXPLORER_PORT=$(parseNixopsPort explorerRpcPort)
