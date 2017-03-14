{ config, lib, pkgs, ... }: with lib;
{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  boot.initrd.luks.devices = [{
    name="root";
    device = "/dev/sdc3";
    preLVM = true;
  }];

  boot.loader.grub.device = "/dev/sdc";
}
