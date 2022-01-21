{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "67.207.67.2"
      "67.207.67.3"
    ];
    defaultGateway = "157.245.112.1";
    defaultGateway6 = "2604:a880:800:c1::1";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce true;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="157.245.113.145"; prefixLength=20; }
          { address="10.17.0.5"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="2604:a880:800:c1::1e0:7001"; prefixLength=64; }
          { address="fe80::a81c:4bff:feab:944e"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "157.245.112.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = "2604:a880:800:c1::1"; prefixLength = 32; } ];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="aa:1c:4b:ab:94:4e", NAME="eth0"

  '';
}
