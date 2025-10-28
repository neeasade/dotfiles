# Edit this configuration file to define what should be installed on
# Your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

let
  hostname = "guthix";
  shared = import ../../config/shared.nix {inherit hostname unstable; };

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
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # "${builtins.fetchGit { url = "https://github.com/NixOS/nixos-hardware.git";
      #                        rev = "3dac8a872557e0ca8c083cdcfc2f218d18e113b0";
      #                      }}/apple/t2"
      # update with: `sudo nix-channel --update`
      <nixos-hardware/apple/t2>

      (import ../../config/desktop.nix {inherit hostname shared pkgs expr;})
    ];

  services.flatpak.enable = true;

  xdg.portal = { # needed for flatpak
    enable = true;
    config.common.default = "*";
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
  };

  hardware.apple-t2.kernelChannel = "stable";
  hardware.facetimehd.enable = true;

  hardware.firmware = [ 
    (pkgs.stdenvNoCC.mkDerivation ( final: { 
      name = "brcm-firmware";
      src = /etc/nixos/firmware/brcm;
      installPhase = ''
        mkdir -p $out/lib/firmware/brcm
        cp ${final.src}/* "$out/lib/firmware/brcm"
        '';
    }))];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];


  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;

  services.logind.settings.Login.HandleLidSwitch = "poweroff";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = hostname; #
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;



  fonts.packages = sets.fonts-all;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  environment.systemPackages = sets.ui ++ [
                                            pkgs.emacs
                                            # nixmox.packages.oomoxFull
                                            # nixmox.defaultNix
                                           ]
                               ++ (with pkgs; [

                                 qutebrowser
                                 # alttab
                                 # toot

                                 steam

                                 # python lsp
                                 pyright
                                 ruff

                                 eask-cli # emacs linting


                                 godef gopls

                                 # xvfb-run

                                 mpd-mpris
                                 pulseaudio

                                 clj-kondo
                                 netpbm
                                 udiskie
                                 lyrebird
                                 protontricks
                                 (sox.overrideAttrs(old: { enableLame = true;}))
                                 xcolor


                                 logseq


                                 ardour
                                 sass
                                 simplescreenrecorder
                                 anki-bin
                                 anki-sync-server

                                 bitwarden
                                 bitwarden-cli

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
                                 dconf
                                 # qutebrowser
                                 discord

                               ]) ++ (with edge; [
                                 gemini-cli
                                 aider-chat

                                 rbw
                                 rofi-rbw
                                 atuin
                                 yt-dlp
                                 google-chrome
                                 # microsoft-edge
                                 nodejs
                               ]) ++ (with unstable; [
                                 # whisper-cpp
                                 # qutebrowser
                                 kitty
                               ]);

  # https://github.com/tailscale/tailscale/issues/4432#issuecomment-1112819111
  networking.firewall.checkReversePath = "loose";
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
    package = edge.tailscale;
  };

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

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.11"; # Did you read the comment?

  nix.gc = {
    automatic = true;
    randomizedDelaySec = "14m";
    options = "--delete-older-than 30d";
  };
}
