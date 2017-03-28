{ config, pkgs, lib, stable, rolling, neeasade, ...}:
with lib;

{
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
  };

  time.timeZone = "America/Chicago";

  users.extraUsers.neeasade = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers"
    ];
    createHome=true;
    home="/home/neeasade";
    shell="/run/current-system/sw/bin/zsh";
    initialPassword="password";
  };

  environment.systemPackages = [
      stable.curl
      stable.dash
      stable.deluge
      stable.feh
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
      stable.ioquake3
      stable.unclutter
      stable.maim
      stable.slop
      stable.bash-completion
      stable.zsh-completions
      stable.lxappearance
      stable.neofetch
      stable.libnotify

      # these weren't found?
      #stable.password-store
      #rolling.xwininfo

      stable.nix
      stable.nix-repl
      stable.nix-prefetch-scripts

      stable.binutils
      stable.bc
      stable.gitAndTools.gitFull
      stable.wget

      rolling.bevelbar
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
      rolling.sxhkd
      rolling.colort
      rolling.mpvc

      rolling.neovim
      rolling.emacs
      rolling.vim

      neeasade.bspwm
      neeasade.xst
  ];

  # todo: consider:
  #system.activationScripts.dotfiles = stringAfter [ "users" ] ''
    #export USER_HOME=${users.extraUsers.neeasade.home}
  #'';
}
