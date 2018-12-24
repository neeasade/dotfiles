# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import ./packages.nix {inherit config pkgs lib; })
      (import ./services.nix {inherit config pkgs; })
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "erasmus";
  # networking.wireless.enable = true;  # wpa_supplicant.

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

  time.timeZone = "America/Chicago";

  users.extraUsers.neeasade = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "video" "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers"
    ];
    createHome=true;
    home="/home/neeasade";
    shell="/run/current-system/sw/bin/zsh";
    initialPassword="password";
  };

  nixpkgs.config.allowUnfree = true;

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";
  # nix.useSandbox = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
