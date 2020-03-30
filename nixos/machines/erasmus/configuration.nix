{ config, pkgs, lib, ... }:

let
  consts = import ../../shared/consts.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../config/packages.nix
      ../../config/services.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.xserver.videoDrivers = [ "nvidia" ];

  boot.extraModulePackages = [ config.boot.kernelPackages.rtlwifi_new ];
  # boot.initrd.kernelModules = [ "wl" ];
  # boot.kernelModules = [ "kvm-intel" "wl" ];
  # boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  networking.hostName = "erasmus";
  # networking.wireless.enable = true;  # wpa_supplicant.
  networking.networkmanager.enable = true;  # wpa_supplicant.

  # todo
  virtualisation = {
    virtualbox = {
      host.enable = false;
    };
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {
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

  time.timeZone = "America/New_York";

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

  # todo: enable auto update maybe
  nixpkgs.config.allowUnfree = true;

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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
