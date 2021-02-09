{ config, pkgs, lib, ... }:

let
  darwin_config = import ./darwin.nix;

  nixcfg = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  # pkgs = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-19.09.tar.gz) { config = nixcfg; };
  stable = pkgs; # controlled by root nix-channel entry
  unstable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) { config = nixcfg; };
  edge = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) { config = nixcfg; };
  # nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};

  # todo: check if this is relative to symlink location
  expr = import ../../nixos/config/expr { inherit pkgs lib unstable edge; };

  # todo: check if this is relative to symlink location
  # consts = import ../../nixos/shared/consts.nix
in {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # todo: cf https://wiki.nikitavoloboev.xyz/package-managers/nix/nix-darwin
  nixpkgs.config = nixcfg;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    (with stable; [

      # sulami's
      adoptopenjdk-hotspot-bin-8
      aspell
      aspellDicts.en
      circleci-cli
      cloc
      clojure
      clojure-lsp
      cmake
      docker
      docker-compose
      emacsMacport
      # emacs
      fd
      fira-code
      git
      gnupg
      gnused
      grpcurl
      isync
      # iterm2
      jq
      leiningen
      miller
      msmtp
      mtr
      netcat
      ngrok
      nodePackages.typescript
      notmuch
      pandoc
      pass
      pinentry_mac
      poetry
      pv
      racket-minimal
      restic
      ripgrep
      rlwrap
      ruby
      sbcl
      skhd
      slack
      stack
      stow
      tmux
      tree
      unrar
      vim
      watch
      wget
      zsh

      # below from: https://github.com/dcarley/dotfiles/blob/master/Brewfile

      # todo: compare against above

      # not available on darwin
      # _1password
      # _1password-gui

      # avrdude
      # aws-vault
      # awscli
      # borkdude/brew/clj-kondo

      # ARST caffeine

      dep

      # docker
      dos2unix

      entr
      # exercism
      # git-filter-repo
      # gnu-sed
      go
      # graphviz
      # helm
      htop
      httpie
      # hub
      # key-codes
      # keybase
      kubernetes kubectl
      minikube
      mitmproxy
      # multimarkdown
      nmap
      # p7zip
      parallel
      # pipenv
      pstree
      # python@3.7
      # qmk-toolbox
      shellcheck
      # spotify
      # sslscan
      # note: there are many terraform providers as well
      # might also consider: terraform-full
      terraform
      # versent/taps/saml2aws
      # visualvm
      wireshark
      yamllint
      youtube-dl
      coreutils

    ]) ++ (with unstable; [
    ]) ++ (with edge; [
    ]) ++ (with expr; [
      pb-git
    ]);


  environment.variables = {
    JAVA_HOME = "/run/current-system/sw";
    ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";
  };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # see above todo
  # users.users.nathan.name = consts.username;
  # users.users.nathan.home = consts.home_directory;
  # networking.computerName = consts.computer_name;
  # networking.hostName = consts.hostname;

  users.users.nathan.name = "nathan";
  users.users.nathan.home = "/Users/nathan";
  networking.computerName = "boodschap";
  networking.hostName = "boodschap";

  # launchd.user.agents = import ./launchd.nix {
  #   pkgs = pkgs;
  #   # home_directory = consts.home_directory;
  #   home_directory = "/Users/nathan";
  # };

  launchd.user.agents = {
    # pkgs = pkgs;
    # home_directory = "/Users/nathan";

    skhd = {
      command = "${pkgs.skhd}/bin/skhd";
      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = true;
      };
    };
  };

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = false;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
