{ config, pkgs, ... }:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  consts = import ../../shared/consts.nix;

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
    createHome = true;
    home = consts.home;
    shell = "/run/current-system/sw/bin/bash";
    initialPassword = "password";
  };

  # environment.systemPackages = (with stable; [
  #   syncthing
  # ]);

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8385";

    # Run as local user
    user = "neeasade";
    dataDir = "/home/neeasade/.local/share/Syncthing";

    declarative = {
      overrideDevices = true;
      devices = builtins.removeAttrs consts.syncthingDevices [ "trouw" ];

      overrideFolders = true;

      folders.main = {
        enable = true;
        path = "${consts.home}/sync/main";
        devices = [ "erasmus" "geloof" ];
      };

      folders.orgzly = {
        enable = true;
        path = "${consts.home}/sync/orgzly";
        devices = [ "erasmus" "geloof" "phone"];
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
