{ config, pkgs, ... }:

let
  # 17.03 channel
  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-16.09.tar.gz) {};

  # unstable channel
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};

  # personal channel 
  neeasade = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/nixos-17.03.tar.gz) {};
in
{
imports = [ 
    ./hardware-configuration.nix
    ./boot.nix
    ./base.nix
    ./services.nix
  ];

   networking.hostName = "littleapple"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # List services that you want to enable:
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [
    #(with stable; [
      stable.curl
      stable.dash
      stable.deluge
      stable.feh
      stable.fira-code
      stable.firefox
      stable.ghostscript
      stable.gimp
      stable.inkscape
      stable.jq
      stable.leafpad
      stable.libreoffice 
      stable.libtiff
      stable.mpd
      stable.mpv
      stable.mupdf
      stable.ncmpcpp
      stable.ntfs3g
      stable.p7zip
      stable.pciutils
      stable.stow
      stable.sudo
      stable.tmux
      stable.unzip
      stable.wget 
      stable.zlib
      stable.zsh
  
      stable.nix
      stable.nix-repl
      stable.nix-prefetch-scripts
  
      stable.binutils
      stable.bc
      stable.gitAndTools.gitFull
      stable.wget
      stable.gcc
    #])
    #(with rolling; [
      rolling.compton
      rolling.dmenu2
      rolling.dunst
      rolling.dzen2
      rolling.firefox
      rolling.i3lock
      rolling.lemonbar-xft
      rolling.pcmanfm
      rolling.qutebrowser
      rolling.rxvt_unicode
      rolling.xdo 
      rolling.xdotool 
      rolling.xtitle
  
      rolling.neovim
      rolling.emacs
      rolling.vim
    #])
    #(with neeasade; [
      neeasade.bspwm
      neeasade.colort
      neeasade.xst
    #])
  ];

  services = {
    xserver = {
      enable = true;
      layout = "us";

      synaptics = {
        enable = true;
        twoFingerScroll = true;
        tapButtons = false;
        palmDetect = true;
      };

      windowManager = {
        default = "bspwm";
        bspwm = {
          # soon...
          package = neeasade.bspwm;
          enable = true;
        };
      };

      desktopManager = {
        xterm.enable = false; 
        default = "none";
      };

      displayManager.slim = {
        enable = true;
        extraConfig = ''
        session_font Liberation Sans:size=16
        session_color #000000
        '';
      };
    };

    #printing = {
      #enable = true;
      #drivers = [ pkgs.gutenprint pkgs.postscript-lexmark pkgs.splix ];
    #};

    #unclutter= true;
    #dbus.enable = true;
    acpid.enable = true;
    # todo : look into conf of ssh.
    #openssh.enable = true;

	# allow user to pick WM nix-env version
	xserver.autorun = false;
  };


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
