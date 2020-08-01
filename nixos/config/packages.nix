{ config, pkgs, lib, ...}:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  # pkgs = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-19.09.tar.gz) { config = nixcfg; };
  stable = pkgs; # controlled by root nix-channel entry
  unstable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};

  # unstable = pkgs;
  # edge = pkgs;

  expr = import ./expr { inherit pkgs lib unstable edge; };

  # pkgs = stable;
  # edge = rolling;
  # rolling = stable;

  core = (with stable; [

    nix-direnv # note: depends on nixpkgs >20.03

    pinentry_qt5

    nodejs # see repo:debounce.js

    leiningen clojure

    entr
    rofi
    cdparanoia
    cdrkit
    cdrtools
    syncthing
    nmap
    mediainfo
    direnv
    kdeFrameworks.networkmanager-qt
    networkmanager_dmenu
    xorg.xkbcomp
    tldr
    toilet
    cowsay
    fortune
    cmatrix
    cava
    inotify-tools
    irssi
    glxinfo
    xorg.xdpyinfo
    gnupg
    arandr
    aspell
    stalonetray
    networkmanagerapplet
    aspellDicts.en
    bash-completion
    bc
    i3blocks

    # covered by gcc(?)
    # there were a few collisions between the two
    binutils

    cron
    curl
    dash
    expect
    feh
    file
    filezilla
    gitAndTools.gitFull

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
    nix-prefetch-scripts

    wayland
    wlroots
    wayland-protocols

    ntfs3g
    openssl
    telnet
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
    zip
    usbutils
    vim
    vlc
    wget
    wmname
    xclip
    xorg.xev
    xorg.xkbcomp
    xorg.xmodmap
    xfontsel
    xurls

    zathura

    calibre

    zsh

    playerctl

    mesa_drivers
    libGL

    gnome3.gnome-terminal
    gnutls # for circe

    fd ripgrep
    google-chrome
    rxvt_unicode

    yq

    qutebrowser
  ]) ++ (with unstable; [
  # ]) ++ (

    dunst dzen2
    ffmpeg
    i3lock meh
    sxhkd txtw
    x11idle xdotool
    xfce.thunar
    xorg.xprop
    xorg.xwininfo
    xtitle
    youtube-dl
    pinta
  ]) ++ ( with expr; [
    # drawterm
    # oomox
    # foliate
    babashka

    lemonbar
    skroll # todo: maybe zscroll this

    pfetch-neeasade
    colort-git

    pb-git
    mpvc-git
    xst-git
    dmenu
    gtkrc-reload
    neeasade-opt
    txth
    wmutils-core-git
    wmutils-opt-git
    xdo-git

  ]) ++ (with unstable; [
  ]);

  inherit (pkgs) eggDerivation fetchegg;
  extra = (with stable; [
    tiled love_11 luajit

    sqlitebrowser
    pup jo
    screenkey
    byzanz

    # oomox
    # gdk_pixbuf
    # glib.dev
    # gtk-engine-murrine
    # gtk3
    # sassc

    cloc

    pandoc
    imagemagick
    graphviz
    audacity
    bfg-repo-cleaner
    picom
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
    # texlive.combined.scheme-full
  ]);

  games = (with stable; [
    nethack
    # wesnoth
    # dolphinEmu

    mesa_drivers
    mesa_glu

    wine
    # wineStaging
    # wineUnstable
    # (wine.override { wineBuild = "wineWow"; })

    minecraft
    openmw
    drawpile
    steam
  ]) ++ (with unstable; [
    # openmw-tes3mp
    # steam
  ]);

  development = (with stable; [
    # build tools
    meson
    cmake
    ninja
    autoconf
    automake
    gnumake

    # lisps
    sbcl
    lispPackages.quicklisp
    guile

    lua
    luarocks

    clang
    gcc

    # ghc
    go

    # JVM
    gradle
    maven
    jdk8
    stack
    boot

    # rust
    cargo
    rustc
    rustfmt
    rustracer

    # racket
    # janet

    python27
    (python37.withPackages(ps: with ps; [
      pip # sometimes we want user level global stuff anyway maybe
      toot

      # please do the needful
      setuptools
      virtualenv
    ]))

    # approach: use pipenv or pyenv to bring in python packages
    # use nix-shell to get the stuff they depend on/reference pip)

    # python27
    # (python35.withPackages(ps: with ps; [
    #   virtualenv
    #   django
    #   screenkey
    #   libxml2
    #   selenium
    #   # praw
    #   # pyqt5
    # ]))

    # other
    docker
    sqlite
    zeal
    zlib

    mono
    ruby
  ]) ++ (with edge; [
    # joker
    # chickenPackages_5.chicken
    # chickenPackages_5.egg2nix
  ]);

  basefonts = (with pkgs; [
    dejavu_fonts
    corefonts
  ]);

  extrafonts = (with pkgs; [
    # roboto roboto-mono
    # tewi-font siji
    # fira fira-code
    font-awesome-ttf
    noto-fonts # todo: noto-emoji?
    powerline-fonts # includes a 'Go Mono for powerline'
  ]);

in
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # "just give me something pls"
  # environment.systemPackages = core ++ [pkgs.emacsUnstable];
  # fonts.fonts = basefonts;

  environment.systemPackages =
    core ++
    extra ++
    development ++
    games ++
    [pkgs.emacsUnstable] ++
    [];

  # for nix-direnv:
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  fonts.fonts =
    basefonts ++
    extrafonts ++
    [];
}
