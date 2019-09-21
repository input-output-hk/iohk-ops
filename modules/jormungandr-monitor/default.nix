{ python3, makeWrapper, runCommand }:

let
  python = python3.withPackages (ps: with ps; [ netifaces prometheus_client requests ]);
in runCommand "jormungandr-monitor" {
  buildInputs = [ makeWrapper ];
} ''
  makeWrapper ${python}/bin/python $out --add-flags ${./jormungandr-monitor.py}
''
