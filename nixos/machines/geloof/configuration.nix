# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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


  networking.hostName = "geloof";
  # networking.wireless.enable = true;  # wpa_supplicant.
  networking.networkmanager.enable = true;

  programs.gnupg.agent.pinentryFlavor  = "qt";

  virtualisation = {
    virtualbox = {
      host.enable = false;
    };
  };

  console = {
    font =  "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {
    opengl.driSupport = true;
    pulseaudio.enable = true;

    opengl.driSupport32Bit = true;
    # todo: this workaround shouldn't be needed anymore, you were just out of sync
    # with upstream channel
    opengl.extraPackages = with pkgs; [ libva ];

    pulseaudio.support32Bit = true;
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
    createHome = true;
    home = consts.home;
    shell = "/run/current-system/sw/bin/bash";
    initialPassword = "password";
  };

  nixpkgs.config.allowUnfree = true;
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";

  # Use the GRUB 2 boot loader.
  # boot.loader.grub.enable = true;
  # boot.loader.grub.version = 2;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;

  # todo: was there a reason for exposing these here
  networking.firewall.allowedTCPPorts = [ 22 80 ];


  services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8385";

      # Run as local user
      user = consts.user;
      dataDir = "${consts.home}/.local/share/Syncthing";

      declarative = {
        overrideDevices = true;
        devices = builtins.removeAttrs consts.syncthingDevices [ "geloof" ];
        overrideFolders = true;

        folders.main = {
          enable = true;
          path = "${consts.home}/sync/main";
          devices = [ "trouw" "erasmus" ];
        };

        folders.orgzly = {
          enable = true;
          path = "${consts.home}/sync/orgzly";
          devices = [ "trouw" "erasmus" "phone"];
        };
      };
    };

  system.stateVersion = "19.09";
}
