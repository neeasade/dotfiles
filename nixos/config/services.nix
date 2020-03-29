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

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8385";

      # Run as local user
      user = "neeasade";
      dataDir = "/home/neeasade/.local/share/Syncthing";

      # declarative = {
      #   # overrideDevices = true;
      #   # devices = builtins.removeAttrs shared.consts.syncthingDevices [ "computer" ];
      #   # overrideFolders = true;
      #   # folders.main = {
      #   #   enable = true;
      #   #   path = "/home/neeasade/Sync";
      #   #   # devices = [ "droplet" "rpi" "phone" "laptop" ];
      #   # };
      # };
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
