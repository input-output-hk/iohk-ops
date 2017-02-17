let
  lib = (import <nixpkgs> {}).lib;
in lib // (rec {
  # Allows to also generate the key compared to upstream genAttrs
  genAttrs' = names: fkey: fname:
    lib.listToAttrs (map (n: lib.nameValuePair (fkey n) (fname n)) names);

  # If we're generating an AMI, don't set nixops deployment attributes
  generatingAMI = (builtins.getEnv "GENERATING_AMI") == "1";

  # Function to generate DHT key
  genDhtKey = { i }: (builtins.fromJSON (builtins.readFile ./static/dht.json))."node${toString i}";

  accessKeyId = "cardano-deployer";
  region = "eu-central-1";
  
  ec2Keys = {
    resources = {
      ec2KeyPairs.my-key-pair = { inherit accessKeyId; region = "eu-central-1"; };
      ec2KeyPairs.cardano-test-eu = { inherit accessKeyId; region = "eu-central-1"; };
      ec2KeyPairs.cardano-test-us = { inherit accessKeyId; region = "us-west-1"; };
      ec2KeyPairs.cardano-test-asia = { inherit accessKeyId; region = "ap-southeast-1"; };
      ec2KeyPairs.cardano-test-sydney = { inherit accessKeyId; region = "ap-southeast-2"; };
      ec2KeyPairs.cardano-test-sa = { inherit accessKeyId; region = "sa-east-1"; };
      elasticIPs.report-server-ip = { inherit region accessKeyId; };
    };
  };

  volhovmKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRMQ16PB/UvIEF+UIHfy66FNaBUWgviE2xuD5qoq/nXURBsHogGzv1ssdj1uaLdh7pZxmo/cRC+Y5f6dallIHHwdiKKOdRq1R/IWToMxnL/TTre+px6rxq21al9r4lvibelIU9vDn0R6OFZo+pRWyXUm33bQ4DVhwWiSls3Hw+9xRq4Pf2aWy//ey5CUTW+QkVdDIOFQG97kHDO3OdoNuaOMdeS+HBgH25bzSlcMw044T/NV9Cyi3y1eEBCoyqA9ba28GIl3vNADBdoQb5YYhBViFLaFsadzgWv5XWTpXV4Kwnq8ekmTcBkDzoTng/QOrDLsFMLo1nEMvhbFZopAfZ volhovm.cs@gmail.com";
  georgeeeKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCymYrIVeNUd9TUPc3cTdHIAatTg3qPbuTENNNHCKQyM4PPvWE+DzmyVDki07NpBk9Ivge3whklcTpRVTMXs7AFX3YIdIxpvc+XVgKhweqd8H0QZkC4/gsJNVTBuY1ZQ2Ldw/rRmbiA9lx/z3vtoI5p4oLSumP2qd5l/KwjDvj66X8K4KOofkFFEiPqBztQwt+A2Hh6XH5qeakQQm/TFeNL6SU0X0zKRdhjyzYAEa2Nt/Te1KK+Jkof7vZ2YnJ3jQFUhC/yRej4o3MPde0HoEP7L86rm9ORcSyQe4jZJ/d6qXMNFAG/7LfU+3LVJ+T584kHXBm5Jl5rOyX2MngNxLxP georgeee@georgeee-laptop";
  gromakKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDK0/mW1SZPMKqWawAthYKBMgLXxCfkzoAMdnQ3cakfFg5QWmmoV6QemtuaPAhBGcA4b10TPh3zW77zYY6ZnSq60iog15iAZxZByYVqBN+7j7JaQHS/cW3uf8KuVc4c/nYDrf3rj8+K0bkllKsfAM3z1JGFvYbO6UPdsjalGwG6GEvkVbbP4y+g5XG58ylgXMdUxvjWBZZTL0Ao8gc5fSeAIjkvVCM/a5EBG1/q7xTukWF2HXqQWc5/551bwDZIkUWsyUm/lr+EUFQBuwXEQ6uwhqA0MFo1r+8ge0eWB0l+fDMn59eBTBlzUNEMjafqOETJZhRU9ieDJUHEsjYVcBd7 gie1994@gmail.com";
  flyingleafeKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsDciZLdWUQ0elPzeJukUZLPAp/uSepUTNPo1vsK5ow4rf3XMLbHwZmIaw0M9PfSHemHHt9Fp1fzch3spm3zltOGO6bDE8Oy1UyOSV+1mGcvj2z4MuJby7tub69k6DwnlSaSDl6LgREXIAtdRbHI3+mqZWXmQp2kCPtq9Pkzv71+jZVQej/eYsO6tJF20jyP/ul9XDGmRSVizI+eaetL8CXg7tlKXNdn9/aONbzMGcmriDKdCKvxmaAYuyGBipURdmuSMwsQobfoNkvAemCpqnIdrVPjQI+s03GRmC/gecCSjWtysKu8BzlxXYJS7yNOboeZBQO2KXfDwtt6OliJuZ flyingleafe@MacBook-Air-Dmitrij.local";
  martoonKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCaGD+d3lU7s1Enth+sg2TZy1HKM9y+cbYSvsFGSfY1dS7J0D+emk009aJQn27PuoOS4SZI6A+Ol7d4BGL/OWZXzS9BR2a2PHmEs1xRr2EyLn7EmG/2m6UHib74gYxx4orUNkcz5rwJqoaTg/aaa2bfuADsrO6/hM0nuZvMaMj2bPVc51fQ/EwhzM8RBLmkpX6XAaB9Ghw9uvCUW+x+0BGtV+H/MlRu0kwdYFw5otO8UYQTdWULQgvarOU1Ek0SZKZORZSiVVhjo84ESiAhTFqHAozC8QimuhhaoeAnIDfPhGtg4Aen/GBqp4yCr7nF/ZoB9Jy1lXpuhO9VG6V/xC39Pck/nD1Y/V5NmMl4TWtyMEP9Ma6C/CsCSq2iBrMHQ8Uq0W2Bz9lAfQKtAaS3RnbJ0q1WdyFI6Smfiryj+DT3liP7x03W6HTkhn3yWEdtQOE6A1sfz85mIX4WUHCGAQtRz+skpW5t3MHYTmMOyThE8+l66RxQARpfpubLllCB75UjmFcc5ni+Y1MJIK/oDapGymHuZGuYQyIiKokrzp6vUfJhT6UcFBa4oQHWXJwXA7eThMtXkl4witsxG1rZBIsbjZZW1NUz1I6971d1a0cOyYw7jxutVgDVnGmsiQmtDdg4wy1kT5jDl2xvdzV0o5fzguS75jY0fcwIcDZ8W0eEUQ== martoon.391@gmail.com";
  domenKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7CTy+OMdA1IfR3EEuL/8c9tWZvfzzDH9cYE1Fq8eFsSfcoFKtb/0tAcUrhYmQMJDV54J7cLvltaoA4MV788uKl+rlqy17rKGji4gC94dvtB9eIH11p/WadgGORnjdiIV1Df29Zmjlm5zqNo2sZUxs0Nya2I4Dpa2tdXkw6piVgMtVrqPCM4W5uorX8CE+ecOUzPOi11lyfCwLcdg0OugXBVrNNSfnJ2/4PrLm7rcG4edbonjWa/FvMAHxN7BBU5+aGFC5okKOi5LqKskRkesxKNcIbsXHJ9TOsiqJKPwP0H2um/7evXiMVjn3/951Yz9Sc8jKoxAbeH/PcCmMOQz+8z7cJXm2LI/WIkiDUyAUdTFJj8CrdWOpZNqQ9WGiYQ6FHVOVfrHaIdyS4EOUG+XXY/dag0EBueO51i8KErrL17zagkeCqtI84yNvZ+L2hCSVM7uDi805Wi9DTr0pdWzh9jKNAcF7DqN16inklWUjtdRZn04gJ8N5hx55g2PAvMYWD21QoIruWUT1I7O9xbarQEfd2cC3yP+63AHlimo9Aqmj/9Qx3sRB7ycieQvNZEedLE9xiPOQycJzzZREVSEN1EK1xzle0Hg6I7U9L5LDD8yXkutvvppFb27dzlr5MTUnIy+reEHavyF9RSNXHTo57myffl8zo2lPjcmFkffLZQ== ielectric@kaki";
  alanKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBg0dSLJhcG3NtAHwi70UvdsQDy0EzDBmIfbPT2Bi2Aq4kicc3iy6vRduAOFgogdeXSL0ML5DD0KAyxgAo8aOVcGNLqsbfvhDwaQjqTjDS1twy7ZysmoFTKMfQT8k/Qs3GjL4ycEiibweJKvRHU2or7/3t+Owvu3yC56uADg4WpP2VThwACzJbwt39VKmEnf3fpxpXZ2s4/Y8bLpG/8XC0/PBbgSbgj7p5ksPAeJOCNSbhq8/NlGPOeoR/puVobX7HVwf/nfn/Jnsqzx4oZ8cuK9zM5GBi6VQ43awGsXXiTmSW57ql3M6lmBGuZOArSfYIY7PsUSQukqeoGA6E/t1r Alex Vieth";

  devKeys = [ volhovmKey georgeeeKey gromakKey flyingleafeKey martoonKey domenKey alanKey ];
})
