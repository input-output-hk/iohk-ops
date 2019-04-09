self: super: {
  prometheus_2 = (super.callPackage <nixpkgs/pkgs/servers/monitoring/prometheus> {}).buildPrometheus {
    version = "2.8.1";
    sha256 = "0x8w0qdh4lcf19nmdlhvgzpy08c2a932d3k49cjwhi5npcsf858n";
    doCheck = false;
  };
}
