{ config, pkgs, lib, ...}:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  # pkgs = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-19.09.tar.gz) { config = nixcfg; };
  stable = pkgs; # controlled by root nix-channel entry
  unstable = stable;
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  # nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};

  # unstable = pkgs;
  # edge = pkgs;

  expr = import ./expr { inherit pkgs lib unstable edge; };

  # pkgs = stable; # edge = rolling;
  # rolling = stable;

  core = (with stable; [
    picom
    dos2unix
    lsof
    redshift

    nix-direnv # note: depends on nixpkgs >20.03

    pinentry_qt5

    nodejs # see repo:debounce.js

    leiningen clojure

    entr
    rofi
    cdparanoia
    cdrkit
    cdrtools
    # syncthing
    nmap pciutils
    mediainfo
    direnv
    # kdeFrameworks.networkmanager-qt
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
    sharutils

    cron
    curl
    dash
    expect
    feh
    file
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
    mpc_cli
    mpd
    # mpdcron
    mpv
    mu
    mumble
    nix-prefetch-scripts

    wayland
    wlroots
    wayland-protocols

    ntfs3g
    openssl
    # telnet
    inetutils
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

    blueman
    pasystray
    pdftk
  ]) ++ (with unstable; [
  # ]) ++ (
    qutebrowser
    zoom-us

    dunst dzen2
    ffmpeg
    i3lock meh
    sxhkd txtw
    x11idle xdotool
    xfce.thunar
    xorg.xprop
    xorg.xwininfo
    xtitle
    pinta
  ]) ++ ( with expr; [
    # drawterm
    # oomox
    # foliate
    # babashka

    lemonbar
    # skroll # todo: maybe zscroll this

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
  ]) ++ (with edge; [
    (pmenu.overrideAttrs(old: {
      src = builtins.fetchGit {url = "https://github.com/neeasade/pmenu"; ref = "master"; };
    }))

    # pmenu

    # pmenu
    babashka
    kitty
    youtube-dl
  ]);

  extra = (with stable; [
    jgmenu
    mediainfo
    okular
    oil

    tiled love_11 luajit
    filezilla

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

    # obs
    cloc

    pandoc
    imagemagick
    graphviz
    audacity
    bfg-repo-cleaner
    # picom
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
    texlive.combined.scheme-full
  ]);

  games = (with stable; [
    runelite
    nethack
    wesnoth
    dolphinEmu

    mesa_drivers
    mesa_glu

    # jstest
    qjoypad

    wine
    # support 64-bit only
    # (wine.override { wineBuild = "wine64"; })
    # wineStaging
    # wineUnstable
    # (wine.override { wineBuild = "wineWow"; })

    minecraft
    openmw
    # drawpile
    # steam
    # steam-run-native

    # (steam.override {
    # extraPkgs = pkgs: [
    #   pkgs.openssl_1_1
    #   pkgs.libnghttp2
    #   pkgs.libidn2
    #   pkgs.rtmpdump
    #   pkgs.libpsl
    # ];
    # }
    # )

    # steam

  ]) ++ (with unstable; [
    # openmw-tes3mp
    # steam
  ]) ++ (with edge; [
    # (steam.override { extraProfile = ''unset VK_ICD_FILENAMES''; })
    steam
    # ()
    discord
  ]);

  development = (with stable; [
    # perf
    circleci-cli

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

    rpm

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
    # rustracer

    # racket
    # janet

    python27
    python37

    # (python37.withPackages(ps: with ps; [
    #   pip # sometimes we want user level global stuff anyway maybe
    #   toot
    #   beancount

    #   # please do the needful
    #   setuptools
    #   virtualenv
    # ]))

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
    emacs
    docker
    sqlite
    zeal
    zlib

    # mono
    # ruby
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
    fira fira-code
    twemoji-color-font
    # font-awesome-ttf
    font-awesome
    noto-fonts # todo: noto-emoji?
    noto-fonts-cjk
    noto-fonts-emoji
    symbola
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
    # [pkgs.emacsUnstable] ++
    [];

  fonts.fonts =
    basefonts ++
    extrafonts ++
    [];
}
