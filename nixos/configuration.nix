{ config, pkgs, lib, ... }:

let
  nixcfg = {
  allowUnfree = true;

  permittedInsecurePackages = [
    "samba-3.6.25"
  ];
};

  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-17.09.tar.gz) { config = nixcfg; };
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  neeasade = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/nixos-17.03.tar.gz) { config = nixcfg; };
  # bleeding
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };

  # set system level
  pkgs = stable;
  expr = import ./expr { inherit pkgs; };
in
{
  _module.args.expr = import ./expr { inherit pkgs; };

  imports = [
    ./hardware-configuration.nix
    ./boot.nix
    (import ./services.nix {inherit expr config pkgs lib rolling neeasade ; })
    (import ./base.nix {inherit lib expr config pkgs stable rolling neeasade edge; })
    (import ./development.nix {inherit config pkgs stable rolling neeasade; })
  ];

   networking.hostName = "littleapple"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

nixpkgs.config.allowUnfree = true;

  # wine dep
   fonts = {
    fonts = (with pkgs; [
      powerline-fonts
      font-awesome-ttf
      siji
      tewi-font
      font-droid
      fira-code
      fira
      fantasque-sans-mono
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

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";
  # nix.useSandbox = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";
}
