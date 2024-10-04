{ pkgs, edge, expr, ...}:

let
  # eg a vps
  bare = (with pkgs; [
    bc
    curl
    file
    gitAndTools.gitFull
    hfsprogs
    htop
    irssi
    jq
    lsof
    nmap
    ntfs3g
    p7zip
    parallel
    screen
    tmux
    unrar
    unzip
    vim
    wget
  ]);

  # stuff I would be annoyed at missing when I reached for it
  cli = bare ++ (with pkgs; [
    babashka
    bfg-repo-cleaner
    cron
    dash
    # direnv
    # nix-direnv
    dos2unix
    entr
    expect
    fd
    ffmpeg
    fzf
    graphviz
    imagemagick
    inetutils
    inotify-tools
    libnotify
    lm_sensors
    lsof
    mediainfo
    nix-prefetch-scripts
    openssl
    pandoc
    pass
    patchelf
    pciutils
    # perf
    pkg-config
    playerctl
    ponymix
    psmisc
    pup
    ripgrep
    socat
    sqlite
    stow
    tldr
    tree
    xurls
    yq
    zip
    zsh
  ]);

  # Desktop setup
  ui = cli ++ (with pkgs; [
    arandr
    (aspellWithDicts (ds: with ds; [ en en-science en-computers ]))
    # aspell
    # aspellDicts.en
    bash-completion
    blueman
    deluge
    dmenu
    dunst
    dzen2
    feh
    firefox
    glxinfo
    gnupg
    gnutls # for circe
    go-mtpfs
    google-chrome
    gparted
    hsetroot
    i3blocks
    i3lock
    jgmenu
    kitty
    libreoffice
    lxappearance
    maim
    meh
    mpc_cli
    mpd
    mpv
    mumble
    networkmanager_dmenu
    networkmanagerapplet
    # nodejs # see repo:debounce.js
    okular
    pasystray
    pavucontrol
    picom
    pinentry_qt5
    plasma5Packages.networkmanager-qt
    redshift
    rofi
    slop
    stalonetray
    sxhkd
    unclutter
    usbutils
    vlc
    wesnoth
    wmname
    x11idle
    xclip
    xdotool
    xfce.thunar
    xfontsel
    xorg.xdpyinfo
    xorg.xev
    xorg.xkbcomp
    xorg.xkbcomp
    xorg.xmodmap
    xorg.xprop
    xorg.xwininfo
    xdo
    xtitle
    telegram-desktop
    signal-desktop
    (lemonbar-xft.overrideAttrs(old: {src = builtins.fetchGit {url = "https://github.com/neeasade/bar"; ref = "thicc";};}))
    qutebrowser
    # (qutebrowser-qt6.overrideAttrs(old: {src = builtins.fetchGit {url = "https://github.com/qutebrowser/qutebrowser"; ref = "main"; };}))
    (pmenu.overrideAttrs(old: { src = builtins.fetchGit {url = "https://github.com/neeasade/pmenu"; ref = "master";};}))
  ]) ++ (with expr; [
    # mpvc-git
    pfetch-neeasade
    neeasade-opt
    wmutils-core-git
    wmutils-opt-git
    pb
  ]);

  # anything else
  fat = ui ++ (with pkgs; [
    ### GAMES
    runelite
    nethack
    dolphinEmu
    # jstest
    qjoypad
    wine
    # minecraft
    openmw

    ### DEV

    # covered by gcc(?)
    # there were a few collisions between the two
    binutils
    sharutils

    leiningen
    clojure
    # perf
    circleci-cli
    meson
    cmake
    ninja
    autoconf
    automake
    gnumake

    sbcl
    # lispPackages.quicklisp
    guile
    ruby

    luarocks

    gcc

    # ghc
    go

    gradle
    maven
    jdk8
    stack

    cargo
    rustc
    rustfmt
    racket
    janet

    # python = pkgs python 3
    python3

    # (python37.withPackages(ps: with ps; [
    #   pip # sometimes we want user level global stuff anyway maybe
    #   toot
    #   beancount

    #   # please do the needful
    #   setuptools
    #   virtualenv
    # ]))

    # nixpkgs.config.permittedInsecurePackages
    # {
    #   nixpkgs.config.permittedInsecurePackages = [
    #     "python-2.7.18.6"
    #   ];
    # }

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

    ### WHATEVER
    (xst.overrideAttrs(old: {src = builtins.fetchGit {url = "https://github.com/gnotclub/xst"; ref = "master"; };}))
    audacity
    byzanz
    cava
    cdparanoia
    # cdrkit
    cdrtools
    cloc
    cmatrix
    cowsay

    # discord

    docker
    filezilla
    fortune
    gimp
    # gnome3.gedit
    gedit
    gnome3.gnome-terminal
    inkscape
    jo
    leafpad
    libtiff
    love_11
    luajit
    neovim
    # obs-studio
    oil
    pinta
    rpm
    rxvt_unicode
    screenkey
    sqlitebrowser
    texlive.combined.scheme-full
    tiled
    toilet
    wayland
    wayland-protocols
    wlroots
    zathura
    zeal
    zlib
    zoom-us
  ]) ++ ([edge.pegasus-frontend pkgs.gamemode]);

  fonts-core = (with pkgs; [dejavu_fonts corefonts symbola]);

  fonts-all = fonts-core ++ (with pkgs; [
    fira
    fira-code
    font-awesome
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    powerline-fonts # includes a 'Go Mono for powerline'
    roboto-mono
    siji
    tewi-font
    twemoji-color-font
    roboto
  ]);

    # mpvc-git
  # ]) ++ (with edge; [
  #   youtube-dl
  # ]);
in
{
  bare       = bare;
  ui         = ui;
  cli        = cli;
  fat        = fat;
  fonts-all  = fonts-all;
  fonts-core = fonts-core;
}
