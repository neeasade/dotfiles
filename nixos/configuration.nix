{ config, pkgs, lib, ... }:

let
  # 17.03 channel
  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-17.03.tar.gz) {};

  # unstable channel (official)
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  # unstable channel (edge)
  #rolling = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};

  # personal channel
  neeasade = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/nixos-17.03.tar.gz) {};
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

 fonts = {
    fonts = (with pkgs; [
      powerline-fonts
      font-awesome-ttf
      siji
      font-droid
      fira-code
      corefonts
      dejavu_fonts
      #source-code-pro
      roboto
      roboto-mono
      roboto-slab
    ]);
  };

  nixpkgs.config.allowUnfree = true;

  nix.useSandbox = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
