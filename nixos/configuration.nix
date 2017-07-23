{ config, pkgs, lib, ... }:

let
  nixcfg = {allowUnfree = true;};
  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-17.03.tar.gz) { config = nixcfg; };
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  neeasade = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/nixos-17.03.tar.gz) { config = nixcfg; };
  # bleeding
  #rolling = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg };

  # set system level
  pkgs = stable;
in
{
  imports = [
    ./hardware-configuration.nix
    ./boot.nix
    (import ./services.nix {inherit config pkgs lib neeasade; })
    (import ./base.nix {inherit lib config pkgs stable rolling neeasade; })
    (import ./development.nix {inherit config pkgs stable rolling neeasade; })
  ];

   networking.hostName = "littleapple"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

nixpkgs.config.allowUnfree = true;

 fonts = {
    fonts = (with pkgs; [
      powerline-fonts
      font-awesome-ttf
      siji
      tewi-font
      font-droid
      fira-code
      corefonts
      dejavu_fonts
      source-code-pro
      noto-fonts
      roboto
      roboto-mono
      roboto-slab
    ]);
  };

  virtualisation = {
    virtualbox = {
      host.enable = true;
    };
  };

  hardware = {
    opengl.driSupport = true;
    pulseaudio.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio.support32Bit = true;
  };

  nix.useSandbox = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
