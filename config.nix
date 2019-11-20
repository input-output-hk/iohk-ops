{ deployerIP, accessKeyId, clusterName, tlsCert, tlsCertKey }:
with (import <nixpkgs/lib>);

filterAttrsRecursive (n: _: n != "_module") (evalModules {
  modules = [
    # Take defaults
    ./modules/parameters.nix
    # Overlay config values
    <config>
    # ... and route nixops network arguments to where they're expected. I guess some coupling is unavoidable (or is it? 🧐)
    (_: {
      node = { inherit accessKeyId; };
      cluster = { inherit deployerIP tlsCert tlsCertKey; name = clusterName; };
    })
  ] ++ (optional (builtins.tryEval <local-module/parameters.nix>).success <local-module/parameters.nix>);
}).config
