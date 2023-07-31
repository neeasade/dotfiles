# named for the galaxian woman in the latter foundation series

{ config, pkgs, ... }:

let
  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  # for bleeding edge nvidia drivers
  # nixos-edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/d6412390c2b9405db4e9d0fe43677f60a0a5b1a6.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  # nixos-edge = import (fetchTarball https://github.com/neeasade/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  # nixos-edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/22.11.tar.gz) { config = nixcfg; };

  edge-packages = edge.linuxPackages_latest;
  # edge-packages = nixos-edge.linuxPackages_6_1;

  expr = import ../../config/expr/default.nix {inherit pkgs edge;};
  consts = import ../../shared/consts.nix;
in
{
  imports =
    [ 
      ./hardware-configuration.nix
      (import ../../config/packages.nix {inherit pkgs edge expr;})
      (import ../../config/services.nix {inherit pkgs expr;})
    ];

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

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    # package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    # package = config.boot.kernelPackages.nvidiaPackages.production;
  };


  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor  = "qt";

  programs.steam.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "bliss"; # Define your hostname.
  networking.networkmanager.enable = true;  

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

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  security.rtkit.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };


  # services.pipewire = {
  #   enable = true;
  #   alsa.enable = true;
  #   alsa.support32Bit = true;
  #   pulse.enable = true;
  #   # If you want to use JACK applications, uncomment this
  #   #jack.enable = true;

  #   # use the example session manager (no others are packaged yet so this is enabled by default,
  #   # no need to redefine it in your config for now)
  #   #media-session.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.users.neeasade = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "video" "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers" "cdrom" "docker"
    ];
    home = consts.home;
    shell="/run/current-system/sw/bin/bash";
    initialPassword="password";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

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
  services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8385";

      # Run as local user
      user = consts.user;
      dataDir = "${consts.home}/.local/share/Syncthing";

        overrideDevices = true;
        devices = builtins.removeAttrs consts.syncthingDevices [ "bliss" ];

        overrideFolders = true;

        folders.main = {
          enable = true;
          path = "${consts.home}/sync/main";
          devices = [ "trouw" "geloof" ];
        };

        folders.orgzly = {
          enable = true;
          path = "${consts.home}/sync/orgzly";
          devices = [ "trouw" "geloof" "phone"];
        };
    };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
