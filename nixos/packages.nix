{ config, pkgs, lib, ...}:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;

    permittedInsecurePackages = [
      "samba-3.6.25"
    ];
  };

  # todo: use channels for this roll
  stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-19.09.tar.gz) { config = nixcfg; };
  rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };

  #stable = pkgs;
  #rolling = pkgs;
  #edge = pkgs;

  expr = import ./expr { inherit pkgs lib; };

  pkgs = stable;
  # edge = rolling;
  # rolling = stable;

  core = (with stable; [
    # networkmanager
    haskellPackages.xmobar
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
    # network-manager
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

    emacs
    mesa_drivers
    libGL


    # mesa_glu

    (python36.withPackages(ps: with ps; [
      virtualenv
      django
      selenium
      # pyqt5
      praw
    ]))

  ]) ++ (with rolling; [
    colort
    # dmenu2
    # dmenu
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
    xrq
    # xst
    xtitle
    youtube-dl
    qutebrowser
  ]) ++ ( with expr; [
    # qutebrowser-git
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
    # wmgroup
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

    # circe
    gnutls

    ripgrep
    pandoc
    imagemagick
    graphviz
    google-chrome
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
    #minecraft
    #wesnoth
    #dolphinEmu
    # ioquake3
    # minecraft
    # wineUnstable
    mesa_drivers
    mesa_glu
    # samba
    #wine
    # winetricks
    # wineStaging
    # (wineStaging.override { wineBuild = "wineWow"; })
    # winetricks
    # (wine.override { wineBuild = "wineWow"; })
    # (wineStaging.override { wineBuild = "wineWow"; })
    # (wineStaging.override {wineBuild = "wineWow"; pulseaudioSupport = true; pcapSupport = true; gstreamerSupport = true;})
    # configureFlags = "--enable-win64 --with-alsa --with-pulse";
    # crispy-doom
  ]) ++ (with rolling; [
    #openmw
    steam
    # openmw-tes3mp
  ]) ++ (with rolling; [
    #drawpile
  ]);

  development = (with stable; [
    # (python35.withPackages(ps: with ps; [
    #   # screenkey
    #   # libxml2
    #   # selenium
    #   # praw
    # ]))

    meson

    # chicken
    # egg2nix

    # chickenPackages_5.chicken
    # chickenPackages_5.egg2nix
    # chicken-install apropos chicken-doc

    # (egg-chicken-doc =
    # eggDerivation {
    # name = "chicken-doc-4.6.3";

    # src = fetchegg {
    #   name = "numbers";
    #   version = "4.6.3";
    #   sha256 = "0aczzpq6f31lk1919aiywknaci69m1apyx905m2ln2qw8zwmwibq";
    # };

    # buildInputs = [];
    # })

    # (egg-apropos =
    # eggDerivation {

    # })

    sbcl
    lispPackages.quicklisp

    # clang
    lua
    luarocks
    autoconf
    automake
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
    # csi, conflicts with chicken
    # mono
    nodejs
    ruby
    cargo
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
      selenium
      # pyqt5
      praw
    ]))
  ]) ++ (with expr; [
    # boot-new
  ]) ++ (with edge; [
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
  environment.systemPackages =
    core ++
    extra ++
    # development ++
    games ++
    [];

  fonts.fonts =
    basefonts ++
    extrafonts ++
    [];
}
