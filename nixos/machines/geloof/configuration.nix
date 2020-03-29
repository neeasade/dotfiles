# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# todo: make this work for both your old comp and your current comp

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../config/packages.nix
      ../../config/services.nix
    ];


  # boot.extraModulePackages = [ config.boot.kernelPackages.rtlwifi_new ];

  networking.hostName = "bridge";
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
    opengl.extraPackages = with pkgs; [ libva ];
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

  nixpkgs.config.allowUnfree = true;

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";
  # nix.useSandbox = true;


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

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.firewall.allowedTCPPorts = [ 22 80 ];

  system.stateVersion = "19.09";
}
