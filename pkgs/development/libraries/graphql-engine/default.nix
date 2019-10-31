{ haskellPackages }:

haskellPackages.override {
  overrides = self: super: {
    ci-info = self.callPackage ./ci-info.nix {};
    graphql-engine = self.callPackage ./graphql-engine.nix {};
    graphql-parser = self.callPackage ./graphql-parser.nix {};
    #multi-ghc-travis = self.callPackage ./multi-ghc-travis.nix {};
    pg-client = self.callPackage ./pg-client.nix {};
  };
}
