with (import ./../lib.nix);

globals: imports: params:
  { pkgs, nodes, config, resources, options, ...}:
  {
    imports = [
      ./common.nix
      ./amazon-base.nix
      ./network-wide.nix
    ] ++ map (path: import path globals params) imports;

    global.organisation = params.org;

    services.dnsmasq.enable = true;
    services.dnsmasq.servers = [ "127.0.0.1" ];

    # TODO: DEVOPS-8
    #deployment.ec2.ami = (import ./amis.nix).${config.deployment.ec2.region};
    deployment.ec2.region         = mkForce params.region;
    deployment.ec2.zone           = mkForce params.zone;
    deployment.ec2.accessKeyId    = params.accessKeyId;
    deployment.ec2.keyPair        = resources.ec2KeyPairs.${params.keyPairName};
    deployment.ec2.securityGroups =
      with params;
      let sgNames =
           optionals typeIsExplorer              [ "allow-to-explorer-${params.region}" ]
        ++ optionals typeIsFaucet                [ "allow-to-faucet-${params.region}" ]
        ++ optionals typeIsCore                  [ "allow-cardano-static-peers-${params.name}-${params.region}-${params.org}" ]
        ++ optionals typeIsRelay                 [ "allow-kademlia-public-udp-${params.region}"
                                                   "allow-cardano-public-tcp-${params.region}" ]
        ++ optionals config.global.enableEkgWeb  [ "allow-ekg-public-tcp-${params.region}-${params.org}" ];
      in map (resolveSGName resources)
             (if config.global.omitDetailedSecurityGroups
              then [ "allow-all-${params.region}-${params.org}" ]
              else sgNames);

    services.dd-agent.processConfig = let
      processName = if params.typeIsExplorer then "cardano-explorer"
                    else if params.typeIsFaucet then "cardano-faucet"
                    else "cardano-node-simple";
    in ''
      init_config:

      instances:
      - name:            ${processName}
        search_string: ['${processName}']
        exact_match: True
        thresholds:
          critical: [1, 1]
    '';
  }
