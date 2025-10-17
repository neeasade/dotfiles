{ hostname, shared, expr, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  environment.wordlist.enable = true;

  # environment.extraInit = ''
  #   # SVG loader for pixbuf (needed for GTK svg icon themes)
  #   export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
  #   '';

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-qt;

  services.printing = {
    enable = true;
    drivers = [ expr.hl2350 ];
    # drivers = [ pkgs.gutenprint];
  };

  # hardware.bluetooth.enable = true;
 hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Experimental = "true";
      };
      Policy = {
        AutoEnable = "true";
      };
    };
  };

  services.blueman.enable = true;
  # hardware.bluetooth.hsphfpd.enable = true;

  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  services = {
    acpid.enable = true;
    openssh.enable = true;
  };

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = shared.user;
  services.displayManager.defaultSession = "none+bspwm";

  services.xserver = {
    enable = true;
    autorun = true;
    xkb.layout = "us";

    # displayManager.autoLogin.user = shared.user;

    windowManager.bspwm = {
      enable = true;
      package = expr.bspwm-git;
    };

    desktopManager = {
      xterm.enable = true;
    };

    displayManager.lightdm = {
      enable = true;
      greeter.enable = false;
    };
  };

  # for nix-direnv:
  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
  };
  environment.pathsToLink = ["/share/nix-direnv"];

  services.syncthing = shared.syncthingConfig;
}
