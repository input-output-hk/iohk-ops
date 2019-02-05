{ getSrc
, solidity
, iele
, callPackage
}:

(callPackage (getSrc "solidity-service") { inherit solidity iele; })."solidity-service"
