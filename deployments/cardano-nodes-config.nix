{ nodeLimit, ... }:

with (import ./../lib.nix);
mergeNodes [
  (genNodes nodeLimit (range 0 1) (i: { region = "eu-central-1"; inherit i; }))
  (genNodes nodeLimit (range 2 3) (i: { region = "eu-west-1"; inherit i; }))
  (genNodes nodeLimit (range 4 5) (i: { region = "eu-west-2"; inherit i; }))
  (genNodes nodeLimit (range 6 7) (i: { region = "ap-southeast-1"; inherit i; }))
  (genNodes nodeLimit (range 8 9) (i: { region = "ap-southeast-2"; inherit i; }))
  (genNodes nodeLimit (range 10 11) (i: { region = "ap-northeast-1"; inherit i; }))
  (genNodes nodeLimit (range 12 13) (i: { region = "ap-northeast-2"; inherit i; }))
]
