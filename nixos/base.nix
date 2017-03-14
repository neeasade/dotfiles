{ config, lib, pkgs, ...}: with lib;
{
  # todo: virtualbox
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

  #environment.shellAliases = {
    # todo
  #};

  #nixpkgs.config.chromium.enableWideVine = true;
  #nixpkgs.config.chromium.enablePepperFlash = true;

  environment.systemPackages = with pkgs; [
    # Network
    wget
    curl
    #chromium
    firefox
    deluge

    # System
    sudo
    zsh
    dash
    tmux
    fira-code
    unzip
    ghostscript
    mupdf
    libtiff
    p7zip
    ntfs3g
    pciutils
    zlib

    # Gnome
    gnome3.gnome-tweak-tool
    gnome.libgnomecups

    # Media
    mpv
    inkscape
    #gimp

    # Office
    libreoffice
    leafpad

    # Rice
    bspwm
    qutebrowser
    dmenu2
    compton
    lemonbar
    dunst
    stow
    jq
    dzen2
    xdotool
    pcmanfm
    feh
    ncmpcpp
    mpd
    i3lock
    rxvt_unicode
  ];

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

  # todo: consider:
  #system.activationScripts.dotfiles = stringAfter [ "users" ] ''
    #export USER_HOME=${users.extraUsers.neeasade.home}
  #'';
}
