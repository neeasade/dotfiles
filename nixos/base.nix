{ config, pkgs, expr, lib, stable, rolling, neeasade, edge, ...}:

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

  environment.extraInit = ''
    # SVG loader for pixbuf (needed for GTK svg icon themes)
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)

    '';

  environment.systemPackages =
    (with stable; [
    psmisc
    dolphinEmu
    gparted
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
    ponymix

    go-mtpfs

    # allow xst terminfo to have higher priority
    (lib.lowPrio ncurses)
    mupdf
    ncmpcpp
    neofetch
    ntfs3g
    pass
    p7zip
    pciutils
    stow
    tmux
    screen
    unclutter
    unzip
    wget
    wineUnstable
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

    # ghostscript conflict
    (lib.lowPrio texlive.combined.scheme-full)
    zathura
    steam
    socat
    compton
  ]) ++ (with rolling; [
    slop # needs rolling for eval args

    ranger
    x11idle
    meh

    bevelbar
    colort
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
    rxvt_unicode
    sxhkd
    vim
    xdo
    xdotool
    xorg.xwininfo
    xtitle
    txtw
    xrq
    wmutils-core
    youtube-dl
    ffmpeg
    # python conflict?
    (lib.lowPrio qutebrowser)
  ]) ++ (with neeasade; [
    xst
    gtkrc-reload
  ]) ++ (with edge; [])
    ++ ( with expr; [
    wmutils-opt-git
  ]);

  # todo: consider:
  #system.activationScripts.dotfiles = stringAfter [ "users" ] ''
    #export USER_HOME=${users.extraUsers.neeasade.home}
  #'';
}
