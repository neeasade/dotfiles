{ config, pkgs, ...}:

let
  expr = import ./expr { inherit pkgs; };
in
{
  services = {

  #   udev.extraRules = ''
  #   # gamecube wii u usb adapter
  #   ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0337", MODE="666", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device" TAG+="uaccess"
  #   # This rule is needed for basic functionality of the controller in Steam and keyboard/mouse emulation
  #   SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"
  #   # This rule is necessary for gamepad emulation; make sure you replace 'pgriffais' with a group that the user that runs Steam belongs to
  #   KERNEL=="uinput", MODE="0660", GROUP="wheel", OPTIONS+="static_node=uinput"
  #   # Nintendo Switch Pro Controller over USB hidraw
  #   KERNEL=="hidraw*", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="2009", MODE="0666"
  #   # Nintendo Switch Pro Controller over bluetooth hidraw
  #   KERNEL=="hidraw*", KERNELS=="*057E:2009*", MODE="0666"



  #   # NS PRO Controller USB
  #   # KERNEL=="hidraw*", ATTRS{idVendor}=="20d6", ATTRS{idProduct}=="a711", MODE="0660", TAG+="uaccess", GROUP="wheel"
  #   KERNEL=="hidraw*", ATTRS{idVendor}=="20d6", ATTRS{idProduct}=="a711", MODE="0660", TAG+="uaccess"
  # '';

    # lorri = {enable = true;};
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
