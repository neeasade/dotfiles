# named for the galaxian woman in the latter foundation series
{ lib, config, pkgs, ... }:

let
  hostname = "bliss";
  shared = import ../../config/shared.nix {inherit hostname; };

  # for bleeding edge nvidia drivers
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = shared.nixcfg; };
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz) { config = shared.nixcfg; };
  # edge = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  # edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/23.11.tar.gz) { config = shared.nixcfg; };
  old = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz) { config = shared.nixcfg; };

  edge-packages = edge.linuxPackages_latest;

  # edge-packages = pkgs.linuxPackages_latest;

  # rollback
  # edge-packages = edge.linuxPackages_5_15;
  # edge-packages = edge.linuxPackages_6_1;

  expr = import ../../config/expr/default.nix {inherit pkgs edge;};
  sets = import ../../config/packages.nix {inherit pkgs edge expr;};


  # nixmox = builtins.fetchTarball { url = "https://github.com/Sorixelle/nixmox/archive/1e9b569308efbbf61bd4f471803620715eac53cc.tar.gz"; };
in
{
  imports =
    [ 
      ./hardware-configuration.nix
      (import ../../config/desktop.nix {inherit hostname shared pkgs expr;})
      # (import ../../config/factorio.nix {inherit lib hostname shared pkgs expr;})
      # (import ../../config/factorio2.nix {inherit lib hostname shared pkgs expr;})
    ];

  services.flatpak.enable = true;
  xdg.portal = {
    enable = true;
    # config = pkgs.xdg-desktop-portal-gtk;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  programs.noisetorch.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
  };

  services.pgadmin.enable = true;
  services.pgadmin.initialEmail = "foo@bar.com";
  services.pgadmin.initialPasswordFile = pkgs.writeText "pgadminPW" ''
            password
          '';

  # todo: look into this
  services.adguardhome.enable = false;

  services.plex = {
    enable = false;
    openFirewall = true;
    package = edge.plex;
  };

  # https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  services.udisks2.enable = true;

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
    package = edge.tailscale;
  };

  # https://github.com/tailscale/tailscale/issues/4432#issuecomment-1112819111
  networking.firewall.checkReversePath = "loose";

  services.ollama.enable = true;
  services.ollama.acceleration = "cuda";

  environment.wordlist.enable = true;
  environment.systemPackages = sets.fat ++ [expr.proton-ge-custom
                                            pkgs.emacs-unstable
                                            # nixmox.packages.oomoxFull
                                            # nixmox.defaultNix
                                           ]
                               ++ (with pkgs; [

                                 # python lsp
                                 pyright
                                 ruff

                                 eask-cli # emacs linting


                                 godef gopls

                                 xvfb-run

                                 mpd-mpris
                                 pulseaudio

                                 clj-kondo
                                 netpbm
                                 udiskie
                                 lyrebird
                                 protontricks
                                 (sox.overrideAttrs(old: { enableLame = true;}))
                                 xcolor

                                 renpy

                                 logseq


                                 ardour
                                 sass
                                 simplescreenrecorder
                                 anki-bin

                                 bitwarden
                                 bitwarden-cli

                                 obs-studio

                                 farbfeld

                                 # audible
                                 babashka
                                 vscode
                                 colort
                                 enscript
                                 farbfeld
                                 mgba
                                 slurm
                                 vscode

                                 html2text

                                 eww
                                 # qutebrowser
                                 discord

                               ]) ++ (with edge; [

                                 rbw
                                 rofi-rbw
                                 atuin
                                 yt-dlp
                                 google-chrome
                                 # microsoft-edge
                                 teams-for-linux
                                 nodejs
                                 tailscale
                               ]) ++ (with unstable; [
                                 qutebrowser
                                 kitty
                               ]);


  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  fonts.packages = sets.fonts-all;

  # bleeding edge
  # boot.kernelPackages = edge-packages;
  # nixpkgs.config.packageOverrides = pkgs: {
  #   # swap out all of the linux packages
  #   linuxPackages_latest = edge-packages;
  #   nvidia_x11 = edge.nvidia_x11;
  # };

  # comment out to use nouvea
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    # Modesetting is needed for most wayland compositors
    modesetting.enable = true;
    # Use the open source version of the kernel module (only if using 515.43.04+)
    open = true;
    nvidiaSettings = true; # provide nvidia-settings gui

    # package = config.boot.kernelPackages.nvidiaPackages.latest;

    package = config.boot.kernelPackages.nvidiaPackages.production;
    # package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
  };

  # environment.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = expr.proton-ge-custom;

  programs.steam.enable = true;
  # programs.steam.package = edge.steam;
  programs.steam.package = pkgs.steam;

  # programs.steam.package = (pkgs.steam.override {
  #   extraPkgs = (pkgs:  [ pkgs.openssl_1_1 ]);
  # });

  nixpkgs.config.permittedInsecurePackages = [
                "electron-27.3.11"
  #   "openssl-1.1.1w"
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = hostname;
  networking.networkmanager.enable = true;

  networking.firewall.allowedTCPPorts = [
    # mpd
    8000 6600
  ];

  networking.firewall.allowedUDPPorts = [
    2456 2457 # valheim
  ];

  networking.wireguard.enable = false;
  networking.wireguard.interfaces = {
    wg0 = {
      privateKeyFile = "/home/neeasade/wireguard-lazr.conf";
      # ips = [ "10.1.1.9/32" ];
      ips = [ "10.1.1.9/16" ];
      peers = [{
          endpoint = "lazr.space:51820";
          publicKey = "WQ1QP2aUQrR5o1cwS7lip4oRjxMCquFaMW7ZQynsGkc=";
          persistentKeepalive = 25;
          # route traffic on these subnets through wg0:
          allowedIPs = [
            # "10.1.0.0/24"
            "10.1.0.1/16" # one of lazr's machines
                        # "10.1.1.0/24" # user
                        # "0.0.0.0/0" # to route all trafic through this (lazr: "please do not do this")
                       ];
        }];
    };
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  security.rtkit.enable = true;

  users.users.neeasade = shared.defaultUser;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  networking.extraHosts =
    ''
    127.0.0.1 x.com
    127.0.0.1 bsky.app
    # 127.0.0.1 www.youtube.com
    127.0.0.1 www.hulu.com
    # 127.0.0.1 www.amazon.com

    10.1.0.3 andromeda
  '';

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  nix.gc = {
    automatic = true;
    randomizedDelaySec = "14m";
    options = "--delete-older-than 30d";
  };

  system.stateVersion = "23.05";
}
