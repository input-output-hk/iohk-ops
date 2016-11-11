{ pkgs, der ? "cardano-node", text, exe ? true, destination ? "./a.sh" }:

pkgs.runCommand der
  { inherit text exe;
    passAsFile = [ "text" ]; }
  ''
  echo "Ya otrabotal, hozyan ))))"
  n=${destination}
  mkdir -p "$(dirname "$n")"
  echo -n "$text" > "$n"
  (test -n "$exe" && chmod +x "$n") || true
  ''
