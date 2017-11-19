{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./boot.nix
    (import ./packages.nix {inherit config pkgs; })
    (import ./services.nix {inherit config pkgs; })
  ];

  networking.hostName = "littleapple"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  nixpkgs.config.allowUnfree = true;

  virtualisation = {
    virtualbox = {
      host.enable = true;
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
  };

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

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";
  # nix.useSandbox = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";
}
