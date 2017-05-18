with (import ./../lib.nix);

{
  network.description = "Cardano SL";

  report-server = import ./../modules/amazon-base.nix;
}
