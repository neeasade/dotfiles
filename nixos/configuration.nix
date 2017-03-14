# configuration.nix
# why is there no nix indenter emacs indenting is broken on this.

{ config, pkgs, ... }:

{
imports = [ 
    ./hardware-configuration.nix
    ./boot.nix
    ./base.nix
    ./services.nix
    #./development.nix
  ];

   networking.hostName = "littleapple"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
   environment.systemPackages = with pkgs; [
     wget
    vim
    emacs
    zsh
    nix
      nix-repl
  gcc
binutils
  bc
  gitAndTools.gitFull
  firefox
   ];

hardware = {
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
  };


  # List services that you want to enable:
  nixpkgs.config.allowUnfree = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
