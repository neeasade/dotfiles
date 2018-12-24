{ config, pkgs, ...}:

let
  expr = import ./expr { inherit pkgs; };
in
{

    # maybe I will come back to you another day.

    # systemd.services.guix-daemon = {

    # description="Build daemon for GNU Guix";

    # wantedBy=["multi-user.target"];

    # serviceConfig = {
    # ExecStart="/var/guix/profiles/per-user/root/guix-profile/bin/guix-daemon --build-users-group=guixbuild";
    # RemainAfterExit="yes";
    # Environment="GUIX_LOCPATH=/root/.guix-profile/lib/locale";
    # # StandardOutput=syslog
    # # StandardError=syslog
    # TasksMax = "8192";
    # };

    # # See <https://lists.gnu.org/archive/html/guix-devel/2016-04/msg00608.html>.
    # # Some package builds (for example, go@1.8.1) may require even more than
    # # 1024 tasks.
    # };

  services = {
    bitlbee = {
      enable = true;
      plugins = with pkgs; [
        bitlbee-facebook
        bitlbee-steam
      ];

      libpurple_plugins = with pkgs; [
        telegram-purple
        purple-hangouts
      ];
    };

    xserver = {
      dpi = 117;
      enable = true;
      autorun = true;
      layout = "us";

      videoDrivers = [ "nvidia" ];

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
    # openssh.enable = true;
  };
}
