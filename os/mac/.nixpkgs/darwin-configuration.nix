{ pkgs, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    # /* pkgs.emacsMacport */
    # pkgs.emacs
    pkgs.openjdk
    pkgs.coreutils
    pkgs.skhd
    ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
}

