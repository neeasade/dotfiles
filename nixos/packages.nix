{ config, pkgs, lib, ...}:

let
  nixcfg = {
    allowUnfree = true;

    permittedInsecurePackages = [
     "samba-3.6.25"
    ];
  };

  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-17.09.tar.gz) { config = nixcfg; };
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  expr = import ./expr { inherit pkgs lib; };

  pkgs = stable;

  base = (with stable; [
    arandr
    aspell
    aspellDicts.en
    bash-completion
    bc
    binutils
    cron
    curl
    dash
    expect
    feh
    file
    filezilla
    gitAndTools.gitFull
    gnome2.zenity
    go-mtpfs
    gparted
    hfsprogs
    hsetroot
    htop
    imagemagick
    jq
    libnotify
    lm_sensors
    lxappearance
    maim
    mksh
    mpc_cli
    mpd
    mpdcron
    mpv
    mu
    mumble
    neofetch
    nix-prefetch-scripts
    nix-repl
    ntfs3g
    openssl
    p7zip
    parallel
    pass
    patchelf
    pavucontrol
    pkgconfig
    ponymix
    psmisc
    screen
    socat
    stow
    tmux
    tree
    unclutter
    unrar
    unzip
    usbutils
    vim
    vlc
    wget
    wmname
    xclip
    xorg.xev
    xorg.xmodmap
    xurls
    zathura
    zsh

    # needed?
    mesa_drivers
    mesa_glu
  ]) ++ (with rolling; [
    colort
    dmenu2
    dunst
    dzen2
    emacs
    ffmpeg
    i3lock
    lemonbar-xft
    meh
    mpvc
    qutebrowser
    ranger
    slop
    sxhkd
    txtw
    wmutils-core
    x11idle
    xdotool
    xfce.thunar
    xorg.xprop
    xorg.xwininfo
    xrq
    xst
    xtitle
    youtube-dl
  ]) ++ ( with expr; [
    bevelbar
    gtkrc-reload
    wmutils-opt-git
    neeasade-opt
  ]);

  extra = (with stable; [
    # oomox
    gdk_pixbuf
    glib.dev
    gtk-engine-murrine
    gtk3
    sassc

    (chromium.override {enablePepperFlash = true;})
    audacity
    bfg-repo-cleaner
    compton
    deluge
    firefox
    fzf
    gimp
    gnome3.gedit
    inkscape
    leafpad
    libreoffice
    libtiff
    neovim
    pcmanfm
    rxvt_unicode
    texlive.combined.scheme-full
  ]);

  games = (with stable; [
    dolphinEmu
    ioquake3
    minecraft
    steam
    # wineUnstable
    wineStaging
    winetricks
  ]);

  development = (with stable; [
    (python36.withPackages(ps: with ps; [
      virtualenv
      django
    ]))

    autoconf
    automake
    boot
    bundler
    clojure
    cmake
    docker
    gcc
    ghc
    gnumake
    go
    gradle
    guile
    jdk8
    leiningen
    maven
    mono
    nodejs
    ruby
    rustc
    rustfmt
    rustracer
    sqlite
    zeal
    zlib
  ]);

  basefonts = (with pkgs; [
    roboto-mono
    siji
    tewi-font
  ]);

  extrafonts = (with pkgs; [
    corefonts
    dejavu_fonts
    fantasque-sans-mono
    fira
    fira-code
    font-awesome-ttf
    font-droid
    noto-fonts
    powerline-fonts
    roboto
    roboto-slab
    source-code-pro
  ]);

in
{
  environment.systemPackages =
    base ++
    extra ++
    development ++
    games ++
    [];

  fonts.fonts =
    basefonts ++
    extrafonts ++
    [];
}
