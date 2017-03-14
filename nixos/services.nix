{ config, lib, pkgs, ...}: with lib;
{
  services = {
    xserver = {
      enable = true;
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
          enable = true;
        };
      };

      desktopManager = {
        xterm.enable = false; 
        default = "none";
      };

      displayManager.slim = {
        enable = true;
        extraConfig = ''
        session_font Liberation Sans:size=16
        session_color #000000
        '';
      };
    };

    #printing = {
      #enable = true;
      #drivers = [ pkgs.gutenprint pkgs.postscript-lexmark pkgs.splix ];
    #};

    #unclutter= true;
    dbus.enable = true;
    acpid.enable = true;
    # todo : look into conf of ssh.
    #openssh.enable = true;
  };

  systemd.user.services.emacs = {
    description = "Emacs Daemon";
    environment = {
      GTK_DATA_PREFIX = config.system.path;
      SSH_AUTH_SOCK = "%t/ssh-agent";
      GTK_PATH = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";
      NIX_PROFILES = "${pkgs.lib.concatStringsSep " " config.environment.profiles}";
      TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
      ASPELL_CONF = "dict-dir /run/current-system/sw/lib/aspell";
    };
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
      ExecStop = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
      Restart = "always";
    };
    wantedBy = [ "default.target" ];
  };
}
