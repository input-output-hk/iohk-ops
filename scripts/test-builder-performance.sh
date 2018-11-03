#!/bin/sh

run_build() {
        nix-build --no-out-link default.nix -A cardano-sl-wallet-new "$@"
}

run_scenario() {
        scenario=$1
        nix-store --gc
        echo "Running scenario ${scenario}:"
        case ${scenario} in
                serial )            run_build                   --no-build-output;;
                maxcores )          run_build --cores 0         --no-build-output;;
                parallel )          run_build -j auto           --no-build-output;;
                parallel-maxcores ) run_build -j auto --cores 0 --no-build-output;;
                best-logged )       run_build -j auto --cores 0;;
        esac 2>&1 | ts -s | ts -i | tee build."${scenario}"."$(date +%s)"
}

for s in "$@"
do run_scenario "$s"
done
