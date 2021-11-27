
{ config, pkgs, lib, ... }:

let
  darwin_config = import ./darwin.nix;

  # todo: restore local file overlay reference option
  circleci-overlay = (import (builtins.fetchGit {
    url = "https://github.com/circleci/nix-overlay";
    ref = "main";}));

  # skip management of nix-channels
  pkgs = import (fetchTarball "https://github.com/nixos/nixpkgs/archive/nixpkgs-21.05-darwin.tar.gz")
      { config = nixconfig;
        overlays = [circleci-overlay];};

  edge = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") { config = nixconfig; };

  nixconfig = {
    allowUnfree = true;
    oraclejdk.accept_license = true;
  };

  # nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};

  # using this because everytime you change the definition you need to re-grant SIP
  yabai_pin = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3.tar.gz" ) { config = nixconfig; };

  expr = import ../../os_nix/config/expr { inherit pkgs lib edge; };

in {
  nixpkgs.config = nixconfig;
  # nixpkgs.overlays = [circleci-overlay]

  security.pki.certificateFiles = [
    "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
  ];

  environment.systemPackages =
    (with pkgs; [
      babashka

      git
      pup

      aspell aspellDicts.en
      awscli2
      cacert
      circleci-cli
      cloc
      cmake
      coreutils
      dep
      # docker
      # docker-compose
      dos2unix
      emacsMacport
      # emacs
      entr
      fd
      fira-code
      fzf
      gnupg
      gnused
      # go
      graphviz
      grpcurl
      # helm # not supported on darwin
      htop
      httpie
      imagemagick
      jq
      kitty
      kubernetes kubectl
      leiningen
      miller
      minikube
      mitmproxy
      msmtp
      mtr
      netcat
      ngrok
      nmap
      nodePackages.typescript
      nodejs
      notmuch
      pandoc
      parallel
      pass
      pinentry_mac
      pipenv
      poetry
      pstree
      pv
      racket-minimal
      restic
      ripgrep
      rlwrap
      (lowPrio ruby)
      sbcl
      shellcheck
      skhd
      slack
      socat
      stack
      stow
      terraform
      tldr
      tmux
      tree
      unrar
      vim
      watch
      wget
      wireshark
      yamllint
      yq
      zsh

      (python39.withPackages
        (ps: with ps; [
          pip
          setuptools
          virtualenv

          requests
          toml
          dateutil]))

      youtube-dl
      # graalvm8-ce
      # go_1_17
    ]) ++ (with edge; [
      # go
      go_1_17
      # placeholder
      # graalvm8-ce
      # clang_12
    ]) ++ (with expr; [
      # pb-git
      colort-git
      pfetch-neeasade
      prod-tools
    ]) ++ (with pkgs; [
      # overlay packages
      graalvm8-ce
    ]);

  environment.variables = {
    JAVA_HOME = "/run/current-system/sw";
    GIT_SSL_CAINFO="/etc/ssl/certs/ca-certificates.crt";
  };

  # programs.zsh.enable = true;  # manage zsh with nix-darwin

  # environment shim for when the default shell is zsh, but not managing zsh with nix
  environment.etc."zshenv".text = ''
      if [ -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]; then
        . ${config.system.build.setEnvironment}
      fi'';

  launchd.user.agents = {
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
    package = yabai_pin.yabai;
    enableScriptingAddition = false;
  };


  homebrew.enable = true;

  homebrew.taps = [
    "homebrew/cask"
    "homebrew/core"
    "homebrew/services"
  ];

  homebrew.brews = [];

  homebrew.casks = [
    # "go"
    "docker"
  ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}


