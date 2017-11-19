{ config, pkgs, ...}:

let
  expr = import ./expr { inherit pkgs; };
in
{
  services = {
    xserver = {
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
        default = "bspwm";
        bspwm = {
          package = expr.bspwm-git;
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
    #openssh.enable = true;
  };
}
