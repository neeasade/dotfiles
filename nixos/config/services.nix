{ config, pkgs, ...}:

let
  expr = import ./expr { inherit pkgs; };
in
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
      dpi = 117;
      enable = true;
      autorun = true;
      layout = "us";

      synaptics = {
        enable = true;
        twoFingerScroll = true;
        tapButtons = false;
        palmDetect = true;
      };

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

      displayManager.lightdm = {
        enable = true;
      };
    };

    printing = {
      enable = true;
      drivers = (with pkgs; [ gutenprint splix ]);
    };

    acpid.enable = true;

    # todo : look into conf of ssh.
    openssh.enable = true;
  };
}
