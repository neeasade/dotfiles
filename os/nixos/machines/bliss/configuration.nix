# named for the galaxian woman in the latter foundation series
{ lib, config, pkgs, ... }:

let
  hostname = "bliss";
  shared = import ../../config/shared.nix {inherit hostname; };

  # for bleeding edge nvidia drivers
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = shared.nixcfg; };
  # edge = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  # edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/22.11.tar.gz) { config = nixcfg; };

  edge-packages = edge.linuxPackages_latest;
  # edge-packages = edge.linuxPackages_5_15;
  # edge-packages = edge.linuxPackages_6_1;

  expr = import ../../config/expr/default.nix {inherit pkgs edge;};
  sets = import ../../config/packages.nix {inherit pkgs edge expr;};
in
{
  imports =
    [ 
      ./hardware-configuration.nix
      (import ../../config/desktop.nix {inherit hostname shared pkgs expr;})
    ];

  services.plex = {
    enable = true;
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

  services.tailscale.enable = true;

  environment.systemPackages = sets.fat ++ [expr.proton-ge-custom pkgs.emacs-unstable]
                               ++ (with pkgs; [
                                 tailscale
                                 udiskie
                                 bitwarden
                                 bitwarden-cli
                               ]);

  fonts.fonts = sets.fonts-all;

  nixpkgs.config.packageOverrides = pkgs: {
    # swap out all of the linux packages
    linuxPackages_latest = edge-packages;
    nvidia_x11 = edge.nvidia_x11;
  };

  # line up your kernel packages at boot
  boot.kernelPackages = edge-packages;

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    # Modesetting is needed for most wayland compositors
    modesetting.enable = false;
    # Use the open source version of the kernel module (only if using 515.43.04+)
    open = false;
    nvidiaSettings = true; # provide nvidia-settings gui
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    # package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
  };

  environment.sessionVariables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = expr.proton-ge-custom;

  programs.steam.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = hostname;
  networking.networkmanager.enable = true;

  networking.firewall.allowedTCPPorts = [
    # mpd
    8000 6600
  ];

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
  services.xserver = {
    layout = "us";
    xkbVariant = "";
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

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  system.stateVersion = "23.05"; 
}
