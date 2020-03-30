{ config, pkgs, ... }:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  unstable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  stable = pkgs; # controlled by root nix-channel entry
in
{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "trouw";
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [
    8385
  ];
  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCojiWaypftE0EGubAGZbXlEh6L1ehNWiD2NiwhTk6sVzHrXJEy1GB3kFep5wCnm0iV+ks8xdKZQBCdeCswfGGWMfnHicdarHEGYuF8uhU7MW0m9IfK7W80lJfhclyf0cBI+N3CL7zw6cDYmjThKARUP0X8iusViyx2hWQF+6vnV92+ak0xbOC5OTJWx27PC6LzTKIrJZdYX7AF4V/NvyiDfw8+BPNCijhcj8hiKFjYyHWGAwD6R6/qxVR39F8Clqg1ygyIgB5g+RaRN0LyLorSEuucS1oigEhkSZ4m+j1CMsHanXuhgxGjbDpdhVw8BNcL7PYvImB4xtmV41qlWR9r neeasade@littleapple"
  ];

  users.extraUsers.neeasade = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "video" "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers"
    ];
    createHome=true;
    home="/home/neeasade";
    shell="/run/current-system/sw/bin/bash";
    initialPassword="password";
  };

  environment.systemPackages = (with stable; [
    syncthing
  ]);

  # syncthingDevices = {
  #     laptop = {
  #       id = "ILTIRMY-JT4SGSQ-AWETWCV-SLQYHE6-CY2YGAS-P3EGWY6-LSP7H4Z-F7ZQIAN";
  #       introducer = true;
  #     };
  #     computer = {
  #       id = "PPZ6RHN-2WYP3HI-OMQKLHY-4LZW6RK-L6CFPK6-G4QTVPV-SUIZH6S-F5E72QB";
  #       introducer = true;
  #     };
  #     droplet = {
  #       id = "4JBUWER-ECEJGT7-XH6NFJB-F4WBHP2-CPREUK6-ETHPHHU-LXGPP3O-IAYLNAI";
  #       addresses = [ "tcp://krake.one:22000" ];
  #     };

  # note: my phone: NQOFNTU-DZ3HCGL-P25F5G2-NBIKBON-AMGH2LV-FZGJR6W-YEAVVDY-ICARSA2
  # my vps: NGZND66-ZLQFRIH-M6W77CS-FTHYQWT-EGYGD6S-V6MGTS2-K7JUCWC-W4DTTAL

  # orange (vps) - NGZND66-ZLQFRIH-M6W77CS-FTHYQWT-EGYGD6S-V6MGTS2-K7JUCWC-W4DTTAL
  # bridge (laptop) -
  # erasmus (desktop) - 4MS4KGQ-X6RBE6E-XX6AOPH-VJXFZ7P-VJ7JC53-35PHBDS-NJK4JRO-LPWHPAN
  # phone NQOFNTU-DZ3HCGL-P25F5G2-NBIKBON-AMGH2LV-FZGJR6W-YEAVVDY-ICARSA2

  #     phone.id  = "O7H6BPC-PKQPTT4-T4SEA7K-VI7HJ4K-J7ZJO5K-NWLNAK5-RBVCSBU-EXDHSA3";
  #     rpi.id    = "KFJ55KX-GEL7PFY-4KBSZG4-WEIUGTV-ICE52PD-PTFUZDV-5PSUOKH-CMNNPQ4";
  #     school.id = "6YYJM7K-ZP3CXHB-P4KU6CF-PVF4RG4-MFGBGXG-CUGZ26X-Z42TS6Q-BVHWKQP";
  #   };

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8385";

    # Run as local user
    user = "neeasade";
    dataDir = "/home/neeasade/.local/share/Syncthing";

    declarative = {
      # overrideDevices = true;
      # devices = builtins.removeAttrs shared.consts.syncthingDevices [ "computer" ];
      overrideFolders = true;
      folders.main = {
        enable = true;
        path = "/home/neeasade/Sync";
        # devices = [ "droplet" "rpi" "phone" "laptop" ];
      };
    };

  };

  # services.postgresql = {
  #   enable = true;
  #   package = pkgs.postgresql_11;
  #   enableTCPIP = true;

  #   authentication = pkgs.lib.mkOverride 11 ''
  #     local all all trust
  #     host all all ::1/128 trust
  #   '';

  #   # initialScript = pkgs.writeText "backend-initScript" ''
  #   #   CREATE ROLE nixcloud WITH LOGIN PASSWORD 'nixcloud' CREATEDB;
  #   #   CREATE DATABASE nixcloud;
  #   #   GRANT ALL PRIVILEGES ON DATABASE nixcloud TO nixcloud;
  #   # '';

  # };
}
