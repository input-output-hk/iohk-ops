{
  last # run for which plots will be created
}:

with import <nixpkgs> {};
writeScriptBin "create-plots.sh" ''
  #! /usr/bin/env nix-shell
  #! nix-shell -i bash -p rWrapper R rPackages.dplyr rPackages.ggplot2 rPackages.gplots rPackages.anytime rPackages.gridExtra

  set -e        # exit on error
  set -o xtrace # print commands

  src=../../scripts/plots.r

  cd ./experiments/${last}
  mkdir -p plots

  Rscript $src ${last}

  mv *.png ./plots/
  mv *.pdf ./plots/
  rm bench-settings2
''
