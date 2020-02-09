{ config, pkgs, lib, ...}:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;

    permittedInsecurePackages = [
      "samba-3.6.25"
    ];
  };

  # stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-19.09.tar.gz) { config = nixcfg; };
  stable = pkgs; # controlled by root nix-channel entry
  unstable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };

  #unstable = pkgs; # force everyone to be on the same level
  #edge = pkgs;

  expr = import ./expr { inherit pkgs lib; };

  # pkgs = stable;
  # edge = rolling;
  # rolling = stable;

  core = (with stable; [
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
    gnome3.zenity

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

    # nix-repl
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
    xurls
    zathura
    zsh

    # emacs
    mesa_drivers
    libGL

    gnutls # for circe
  ]) ++ (with unstable; [
    lorri

    emacs

    # (emacsWithPackages(e: with e; [
    #   emacs-libvterm
    #   # proof-general
    #   # (pdf-tools.overrideAttrs (attrs: { src = pdf-tools-src; }))
    # ]))

    dunst
    dzen2
    ffmpeg
    i3lock
    lemonbar-xft
    meh
    ranger
    sxhkd
    txtw
    x11idle
    xdotool
    xfce.thunar
    xorg.xprop
    xorg.xwininfo
    xtitle
    youtube-dl
    qutebrowser
    pinta

    polybar
  ]) ++ ( with expr; [
    # qutebrowser-git

    colort-git

    # drawterm
    # oomox

    pb-git
    mpvc-git
    xst-git
    dmenu
    bevelbar
    gtkrc-reload
    neeasade-opt
    txth
    wmutils-core-git
    wmutils-opt-git
    xdo-git
  ]) ++ (with edge; [
    # emacs
    # allow images to display
    # (emacs.override { imagemagick = pkgs.imagemagickBig; } )
  ]);

  inherit (pkgs) eggDerivation fetchegg;
  extra = (with stable; [
    # oomox
    gdk_pixbuf
    glib.dev
    gtk-engine-murrine
    gtk3
    sassc

    cloc

    # damn rust really crept in there
    fd
    ripgrep

    pandoc
    imagemagick
    graphviz
    google-chrome
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
    # minecraft
    # wesnoth
    # dolphinEmu

    mesa_drivers
    mesa_glu

    # wine
    # wineStaging
    # wineUnstable
    # (wine.override { wineBuild = "wineWow"; })
  ]) ++ (with unstable; [
    openmw
    steam
    # openmw-tes3mp
    drawpile
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
    leiningen
    clojure
    boot

    # rust
    cargo
    rustc
    rustfmt
    rustracer

    python37
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
    nodejs
    ruby
  ]) ++ (with edge; [
    # lorri

    # joker
    # boot
    # chickenPackages_5.chicken
    # chickenPackages_5.egg2nix
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
    noto-fonts
    powerline-fonts
    roboto
    roboto-slab
    source-code-pro
  ]);

in
{
  # "just give me something pls"
  # environment.systemPackages = core
  # fonts.fonts = basefonts

  environment.systemPackages =
    core ++
    extra ++
    development ++
    games ++
    [];

  fonts.fonts =
    basefonts ++
    extrafonts ++
    [];
}
