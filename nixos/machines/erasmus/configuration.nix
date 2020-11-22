{ config, pkgs, lib, ... }:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };

  consts = import ../../shared/consts.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../config/packages.nix
      ../../config/services.nix
    ];

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor  = "qt";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    enableTCPIP = true;

    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
    '';

    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE localdev WITH LOGIN PASSWORD 'localdev' CREATEDB;
      CREATE DATABASE localdev;
      GRANT ALL PRIVILEGES ON DATABASE localdev TO localdev;
    '';
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = true;

  # boot.initrd.kernelModules = [ "wl" ];
  # boot.kernelModules = [ "kvm-intel" "wl" ];
  # boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  # boot.kernelPackages = edge.linuxPackages_zen;

  networking.hostName = "erasmus";
  # networking.wireless.enable = true;  # wpa_supplicant.
  networking.networkmanager.enable = true;  # wpa_supplicant.

  # todo
  virtualisation = {
    virtualbox = {
      host.enable = false;
    };
    docker = {
      enable = true;
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {

    bluetooth.enable = true;
    opengl.driSupport = true;
    pulseaudio.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio.support32Bit = true;

    #bumblebee.enable = true;
    # nvidia testing pls
    # nvidiaOptimus.disable = true;
    # install nvidia drivers in addition to intel one
    # opengl.extraPackages = [ nvidia.out ];
    # opengl.extraPackages32 = [ nvidia32.out ];
  };

  environment.extraInit = ''
    # SVG loader for pixbuf (needed for GTK svg icon themes)
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
    '';

  time.timeZone = "America/Chicago";

  users.extraUsers.neeasade = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "video" "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers" "cdrom" "docker"
    ];
    createHome=true;
    # home="/home/neeasade";
    home = consts.home;
    shell="/run/current-system/sw/bin/bash";
    initialPassword="password";
  };

  # todo: enable auto update maybe
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseAudio = true;

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";

  # nix.useSandbox = true;

  services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8385";

      # Run as local user
      user = consts.user;
      dataDir = "${consts.home}/.local/share/Syncthing";

      declarative = {
        overrideDevices = true;
        devices = builtins.removeAttrs consts.syncthingDevices [ "erasmus" ];

        overrideFolders = true;

        folders.main = {
          enable = true;
          path = "${consts.home}/sync/main";
          devices = [ "trouw" "geloof" ];
        };

        folders.orgzly = {
          enable = true;
          path = "${consts.home}/sync/orgzly";
          devices = [ "trouw" "geloof" "phone"];
        };
      };
    };



  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 22 ];
  # todo: have this read from file/togglable
  networking.extraHosts =
    ''
    # 127.0.0.1 twitter.com
    # 127.0.0.1 www.twitter.com
  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
