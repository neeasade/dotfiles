{ pkgs, expr, ...}:

{
  # for nix-direnv:
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  services = {
    blueman = {
      enable = true;
    };

    tlp = {
      enable = true;
    };

    xserver = {
      # dpi = 117;
      enable = true;
      autorun = true;
      layout = "us";

      # todo: this only for laptop
      # synaptics = {
        # enable = true;
        # twoFingerScroll = true;
        # tapButtons = false;
        # palmDetect = true;
      # };

      windowManager = {
        bspwm = {
          package = expr.bspwm-git;
          enable = true;
        };
      };

      desktopManager = {
        xterm.enable = true;
      };

      displayManager.defaultSession = "none+bspwm";
      displayManager.autoLogin.user = "neeasade";
      displayManager.autoLogin.enable = true;

      displayManager.lightdm = {
        enable = true;
        greeter.enable = false;
      };
    };

    printing = {
      enable = true;
      drivers = [ expr.hl2350 ];
      # drivers = (with pkgs; [ gutenprint ]);
    };

    acpid.enable = true;

    # todo : look into conf of ssh.
    openssh.enable = true;
  };
}
