# laptop
{ config, pkgs, ... }:

let
  hostname = "geloof"
  shared = import ../../config/shared.nix;
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = shared.nixcfg; };
  expr = import ../../config/expr/default.nix {inherit pkgs edge;};
  sets = import ../../config/packages.nix {inherit pkgs edge expr;};
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import ../../config/desktop.nix {inherit shared hostname pkgs expr;})
    ];

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    tapButtons = false;
    palmDetect = true;
  };

  services.tlp.enable = true;
  environment.systemPackages = sets.ui ++ [pkgs.emacs];
  fonts.fonts = sets.fonts-core;

  networking.hostName = hostname;
  networking.networkmanager.enable = true;

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

  time.timeZone = "America/New_York";

  users.extraUsers.neeasade = shared.defaultUser;

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";

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
  networking.firewall.allowedTCPPorts = [ 22 80 ];

  system.stateVersion = "19.09";
}
