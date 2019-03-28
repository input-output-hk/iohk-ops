{ getSrc
, solidity
, iele-semantics
, callPackage
}:

(callPackage (getSrc "solidity-service") { inherit solidity; iele = iele-semantics; })."solidity-service"
