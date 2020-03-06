{ config, pkgs, ...}:

let
  expr = import ./expr { inherit pkgs; };
in
{
  services = {
    # lorri = {enable = true;};

    tlp = {
      enable = true;
    };

    xserver = {
      dpi = 117;
      enable = true;
      autorun = true;
      layout = "us";

      # videoDrivers = [ "nvidia" ];

      synaptics = {
        enable = true;
        twoFingerScroll = true;
        tapButtons = false;
        palmDetect = true;
      };

      windowManager = {
        default = "bspwm";
        bspwm = {
          package = expr.bspwm-git;
          #package = pkgs.bspwm;
          enable = true;
        };
      };

      desktopManager = {
        xterm.enable = true;
        default = "none";
      };

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
