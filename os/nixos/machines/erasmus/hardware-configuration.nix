# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # todo: https://github.com/NixOS/nixpkgs/issues/31260 ?
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/af237de7-fdd4-4cc2-8db3-f8fc33b9a5c6";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/8CC6-209C";
      fsType = "vfat";
    };

  # fileSystems."/media/LIVEWIRE" =
  #   { device = "/dev/disk/by-uuid/2cf891b0-4adf-4fa0-bd80-e335303a1b13";
  #     fsType = "btrfs";
  #     options = [ "nofail" ];
  #   };

  # fileSystems."/media/CODERED" =
  #   { device = "/dev/disk/by-uuid/8c484b3d-b026-41f9-9cbe-dd551d1aead3";
  #     fsType = "ext4";
  #   };

  fileSystems."/media/VOLTAGE" =
    { device = "/dev/disk/by-uuid/00ebaffa-6c52-4422-8168-4a84622bad0c";
      fsType = "btrfs";
      options = [ "nofail" ];
    };

  # fileSystems."/media/KICKSTART" =
  #   { device = "/dev/disk/by-uuid/84894ed7-8e80-49a5-8951-ac61c23a6564";
  #     fsType = "btrfs";
  #     options = [ "nofail" ];
  #   };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/8d5ee437-0eed-4723-8692-8c85305058f0"; }
    ];

  nix.settings.max-jobs = lib.mkDefault 6;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
