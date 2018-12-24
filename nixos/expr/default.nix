{ pkgs ? import <nixpkgs> {}, ... }: with pkgs;

let
  # I'm not sure of the correct form here rn
  placeholder = "temp";
in
  rec {
    gtkrc-reload = callPackage ./gtkrc-reload {};

    bevelbar = (pkgs.bevelbar.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://www.uninformativ.de/git/bevelbar.git"; ref = "master"; };
        }));

    xst-git = (pkgs.xst.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/xst"; ref = "master"; };
        }));

    txth = (pkgs.txtw.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/txth"; ref = "master"; };
        }));

    bspwm-git = (pkgs.bspwm.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/baskerville/bspwm"; ref = "master"; };
        }));

    qutebrowser-git = (pkgs.qutebrowser.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/qutebrowser/qutebrowser"; ref = "master"; };
        }));

    wmutils-opt-git = (pkgs.wmutils-opt.overrideAttrs(old: {
        buildInputs = old.buildInputs ++ [ pkgs.xorg.xcbutil ];
        src = builtins.fetchGit {url = "https://github.com/wmutils/opt"; ref = "master"; };
        }));

    wmutils-core-git = (pkgs.wmutils-opt.overrideAttrs(old: {
        buildInputs = old.buildInputs ++ [ pkgs.xorg.xcbutil ];
        src = builtins.fetchGit {url = "https://github.com/wmutils/core"; ref = "master"; };
        }));

    neeasade-opt = (pkgs.wmutils-opt.overrideAttrs(old: {
        buildInputs = old.buildInputs ++ [ pkgs.xorg.xcbutil ];
        src = builtins.fetchGit {url = "https://github.com/neeasade/opt"; ref = "master"; };
        }));

    xdo-git = (
        pkgs.xdo.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/baskerville/xdo"; ref = "master"; };
        }));
}
