{ config, pkgs, lib, stable, rolling, neeasade, ...}:
with lib;
{
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

  environment.systemPackages =
  (with stable; [
(chromium.override {enablePepperFlash = true;})
tree
hfsprogs
    parallel
    pango
    aspell
    aspellDicts.en
    bash-completion
    bc
    xorg.xmodmap
    binutils
    curl
    dash
    deluge
    feh
    ghostscript
    gimp
    gitAndTools.gitFull
    inkscape
    ioquake3
    jq
    leafpad
    libnotify
    libreoffice
    libtiff
    lxappearance
    maim
    mpd
    mpv
    mupdf
    ncmpcpp
    neofetch
    nix
    nix-prefetch-scripts
    nix-repl
    ntfs3g
    pass
    p7zip
    pciutils
    slop
    stow
    sudo
    tmux
    unclutter
    unzip
    wget
    wget
    wine
    zlib
    zsh
    zsh-completions
    htop
    hsetroot
    xurls
    xclip
    imagemagick

  ]) ++ (with rolling; [

    # lowprio so that ncurses st terminfo doesn't conflict with xst.
    (lib.lowPrio ranger)

    (steam.override {newStdcpp = true; nativeOnly = true;})
    bevelbar
    colort
    compton
    dmenu2
    dunst
    dzen2
    emacs
    firefox
    i3lock
    lemonbar-xft
    mesa_drivers
    mesa_glu
    mpvc
    neovim
    pcmanfm
    qutebrowser
    rxvt_unicode
    sxhkd
    vim
    xdo
    xdotool
    xorg.xwininfo
    xtitle
    weechat
    txtw
    xrq
    wmutils-core
    wmutils-opt
    ffmpeg
  ]) ++ (with neeasade; [
    xst
    bspwm
    #wmutils-opt
  ]);

  # todo: consider:
  #system.activationScripts.dotfiles = stringAfter [ "users" ] ''
    #export USER_HOME=${users.extraUsers.neeasade.home}
  #'';
}
