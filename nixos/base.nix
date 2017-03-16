{ config, lib, pkgs, ...}: with lib;
{
  # todo: virtualbox
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
  };

  time.timeZone = "America/Chicago";

  #environment.shellAliases = {
    # todo
  #};

  users.extraUsers.neeasade = {
    isNormalUser = true;
    uid = 1000;
    extraGroups= [
      "wheel" "disk" "audio" "networkmanager" "systemd-journal" "vboxusers"
    ];
    createHome=true;
    home="/home/neeasade";
    shell="/run/current-system/sw/bin/zsh";
    initialPassword="password";
  };

  # todo: consider:
  #system.activationScripts.dotfiles = stringAfter [ "users" ] ''
    #export USER_HOME=${users.extraUsers.neeasade.home}
  #'';
}
