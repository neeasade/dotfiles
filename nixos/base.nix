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

    # packages for oomox:
    gtk-engine-murrine
    gdk_pixbuf
    glib.dev
    sassc
    gtk3

    arandr
    expect
    cron
    tree
    hfsprogs
    lm_sensors
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
    fzf
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
    mpc_cli
    mpv
    mksh
    mumble
    pavucontrol
    patchelf
    openssl
    pstree
    ponymix

    # allow xst terminfo to have higher priority
    (lib.lowPrio ncurses)
    mupdf
    ncmpcpp
    neofetch
    # unsure how to make these higher priority
    #nix
    #nix-prefetch-scripts
    #nix-repl
    #sudo
    ntfs3g
    pass
    p7zip
    pciutils
    slop
    stow
    tmux
    screen
    unclutter
    unzip
    wget
    wget
    wine
    zlib
    zsh
    htop
    hsetroot
    xorg.xev
    xurls
    xclip
    imagemagick
    gnome2.zenity
    pkgconfig
  ]) ++ (with rolling; [
    ranger
    x11idle

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
    xfce.thunar
    pcmanfm
    (qutebrowser.override {withWebEngineDefault = true;})
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
    youtube-dl
    ffmpeg
  ]) ++ (with neeasade; [
    xst
    bspwm
    gtkrc-reload
    #wmutils-opt
  ]);

  # todo: consider:
  #system.activationScripts.dotfiles = stringAfter [ "users" ] ''
    #export USER_HOME=${users.extraUsers.neeasade.home}
  #'';
}
