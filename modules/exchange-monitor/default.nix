{ python3, makeWrapper, runCommand }:

let
  python = python3.withPackages (ps: with ps; [ prometheus_client requests ]);
in runCommand "exchange-monitor-binance" {
  buildInputs = [ makeWrapper ];
} ''
  makeWrapper ${python}/bin/python $out --add-flags ${./exchange-monitor.py}
''
