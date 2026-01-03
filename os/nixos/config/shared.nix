{hostname, unstable, pkgs, ...}:

rec {
  nixcfg = {
    allowUnfree = true;
    cudaSupport = true;
    oraclejdk.accept_license = true;
  };

  user     = "neeasade";
  home     = "/home/${user}"; # do not add a trailing slash, this sets $HOME (/etc/password)
  dotfiles = "${home}/.dotfiles";

  defaultUser = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "video" "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers" "cdrom" "docker"
    ];
    home = home;
    shell="/run/current-system/sw/bin/bash";
    initialPassword="password";
  };

  sshKeys = [
               # "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCojiWaypftE0EGubAGZbXlEh6L1ehNWiD2NiwhTk6sVzHrXJEy1GB3kFep5wCnm0iV+ks8xdKZQBCdeCswfGGWMfnHicdarHEGYuF8uhU7MW0m9IfK7W80lJfhclyf0cBI+N3CL7zw6cDYmjThKARUP0X8iusViyx2hWQF+6vnV92+ak0xbOC5OTJWx27PC6LzTKIrJZdYX7AF4V/NvyiDfw8+BPNCijhcj8hiKFjYyHWGAwD6R6/qxVR39F8Clqg1ygyIgB5g+RaRN0LyLorSEuucS1oigEhkSZ4m+j1CMsHanXuhgxGjbDpdhVw8BNcL7PYvImB4xtmV41qlWR9r neeasade@littleapple"

               # bliss
               "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC0kxdld4sBji+bWDpuoqnApJp/LPmvRidDa7Ar2Z+HXCwANnwldS0eBzcRH0CzwqDaaYS1Ju4pJaPwRoKbWAR5iRcLe740NyiawzhLN6zfVcpT6b9idN8SODrdntJeX2sPdunZRcviS9clLZuDamqL9jNdMrzXN5M4yR6OSycE2Ntcf/1tLcszxWXg4qM53JD/HLW+nxoT4cBUdplXrnEpuGpnPnQYG2Sf7GKFACgV5f7Cr4Aw/27aQKfkSN7OIUYpew3x1xjIuAmT2RpRZI3R9YLIr6izl/Zb5u7CUBg4UA5dBXkPcN3jAz7IVvnkA76/LsNIkblREiAzLbyY9X/6zK4WStbVwT+aUWHSSHh9DMvQqjt6yobmizS4ALc4c3FFg3OFc5iRt60CmzCTqs6zqyfsxgYgUgnxPyb/HiNSu3FtNlhhtfnRXR7dZBPvpMB1mPK54Hd5CLNk591YvlTqlzNg1tFxVLWkf6zX3/u1mu+yghMDvNMQV6GYbxREr5c= neeasade@nixos"
             ];

  gpgKeys  = {
    signing = "";
  };

  syncthingDevices = {
    bliss = {
      id = "BRUJ4CM-HHG6JMN-XE6QXQ4-V6Y7GYR-YFYHNJF-MENRJRK-BQLS2MX-TONV3Q2";
      introducer = true;
    };

    # macbook (nixos)
    guthix.id = "VHC444Q-RNPECI7-LWWTXFI-DO7UXGG-LQ5M7RL-RTYRFV3-6BP3B6Q-GERVRAB";

    # macbook (macos)
    gilgamesh.id = "5GYJLQE-7PAT6D4-2DQ3CYO-74P4X7V-IB4CAED-2VIXYHC-NV5VDNQ-XHRQDAE";

    # lil box in the closet
    # portal.id = "TAKON4Z-4BDB3HJ-LBRGYCJ-7YLMAFT-P7YYF45-VQRPWJL-TTEWBPF-JBTXRA5";

    # thinkpad
    geloof.id = "5TSVINF-TJQWJOR-4UBDHAH-J3EQ2YU-OY5C5AF-3P25BH5-UKXTMJE-PLSP6QO";

    # vps
    # trouw.id = "NGZND66-ZLQFRIH-M6W77CS-FTHYQWT-EGYGD6S-V6MGTS2-K7JUCWC-W4DTTAL";

    phone = {
      # id = "NQOFNTU-DZ3HCGL-P25F5G2-NBIKBON-AMGH2LV-FZGJR6W-YEAVVDY-ICARSA2";
      # id = "ORECEL6-GP4MDUV-SFLXPI2-3B2KRFS-52VSIT4-S6FG4D2-2Q4Z5Z3-YRRQQQF";
      id = "6C2XA3L-LCMB7AD-ZCZNNCU-4KJN55R-MAYNKLC-CZ4WI5H-WBPP4MA-J3GNAQR";
      # introducer = true;
    };
  };

  # set services.syncthing to enable
  syncthingConfig = {
    package = unstable.syncthing;
    enable = true;
    openDefaultPorts = true;
    guiAddress = "127.0.0.1:8385";

    user = user;
    dataDir = "${home}/.local/share/Syncthing";

    overrideDevices = true;

    settings.devices = (builtins.removeAttrs syncthingDevices [ hostname ]);

    overrideFolders = true;

    settings.folders.main = {
      enable = true;
      path = "${home}/sync/main";
      devices = builtins.attrNames (builtins.removeAttrs syncthingDevices [ hostname "phone" ]);
    };

    settings.folders.orgzly = {
      enable = true;
      path = "${home}/sync/orgzly";
      devices = builtins.attrNames (builtins.removeAttrs syncthingDevices [ hostname ]);
    };
  };
}
