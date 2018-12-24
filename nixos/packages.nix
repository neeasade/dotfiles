{ config, pkgs, lib, ...}:

let
  nixcfg = {
    allowUnfree = true;

    permittedInsecurePackages = [
     "samba-3.6.25"
    ];
  };

  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-18.09.tar.gz) { config = nixcfg; };
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  expr = import ./expr { inherit pkgs lib; };

  pkgs = stable;
  # edge = rolling;
  # rolling = stable;

  core = (with stable; [
    gnupg
    arandr
    aspell
    aspellDicts.en
    bash-completion
    bc
    # covered by gcc(?)
    # there were a few collisions between the two
    # binutils
    cron
    curl
    dash
    expect
    feh
    file
    filezilla
    gitAndTools.gitFull
    # todo: find this
    # gnome2.zenity
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

    # nix-repl
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
    slop
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

    emacs
    # needed?
    # mesa_drivers
    # mesa_glu
  ]) ++ (with rolling; [
    colort
    dmenu2
    # dunst
    dzen2
    ffmpeg
    i3lock
    lemonbar-xft
    meh
    mpvc
    qutebrowser
    ranger
    sxhkd
    txtw
    x11idle
    xdotool
    xfce.thunar
    xorg.xprop
    xorg.xwininfo
    xrq
    # xst
    xtitle
    youtube-dl
  ]) ++ ( with expr; [
    # qutebrowser-git
    xst-git
    bevelbar
    gtkrc-reload
    neeasade-opt
    txth
    wmutils-core-git
    wmutils-opt-git
    xdo-git
    # wmgroup
  ]) ++ (with edge; [
    # emacs
    # allow images to display
    # (emacs.override { imagemagick = pkgs.imagemagickBig; } )
  ]);

  extra = (with stable; [
    # oomox
    gdk_pixbuf
    glib.dev
    gtk-engine-murrine
    gtk3
    sassc

    # (chromium.override {enablePepperFlash = true;})
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
    # texlive.combined.scheme-full
  ]);

  games = (with stable; [
    # dolphinEmu
    # ioquake3
    # minecraft
    # wineUnstable
    mesa_drivers
    mesa_glu
    # samba
    wine
    # winetricks
    # wineStaging
    # (wineStaging.override { wineBuild = "wineWow"; })
    # winetricks
    # (wine.override { wineBuild = "wineWow"; })
    # (wineStaging.override { wineBuild = "wineWow"; })
    # (wineStaging.override {wineBuild = "wineWow"; pulseaudioSupport = true; pcapSupport = true; gstreamerSupport = true;})
    # configureFlags = "--enable-win64 --with-alsa --with-pulse";
  ]) ++ (with stable; [
    steam
  ]);

  development = (with stable; [
    # (python35.withPackages(ps: with ps; [
    #   # screenkey
    #   # libxml2
    #   # selenium
    #   # praw
    # ]))

    # clang
    lua
    luarocks
    autoconf
    automake
    boot
    # comes with ruby already? (collision)
    # bundler
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
    # rustc
    # rustfmt
    # rustracer
    sqlite
    zeal
    zlib
  ]) ++ (with stable; [
    (python36.withPackages(ps: with ps; [
      virtualenv
      django
      pyqt5
      praw
    ]))
  ]);

  basefonts = (with pkgs; [
    roboto-mono
    siji
    tewi-font
  ]);

  extrafonts = (with pkgs; [
  gohufont
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
    core ++
    extra ++
    development ++
    games ++
    [];

  fonts.fonts = basefonts ++ extrafonts ++ [];
}
