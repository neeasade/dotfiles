{ edge, pkgs, ... }: with pkgs;
rec {
  hl2350 = callPackage ./HLL2350DW {};

  proton-ge-custom = stdenv.mkDerivation rec {
    pname = "proton-ge-custom";
    version = "GE-Proton8-9";

    src = fetchurl {
      url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
      sha512 = "b017f55fcfb8f5245c3f025eefdf5797ca5b8131eb892c5470baddd3639abaa189b8c9b46ea1f56c4a5683028d0c8f4840ae5fc845462e1ddecfc4a086c3d46b";
    };

    installPhase = ''mkdir -p $out; mv * $out/ '';
  };

  mpvc-git = stdenv.mkDerivation rec {
    pname = "mpvc";
    version = "git";

    src = builtins.fetchGit {url = "https://github.com/lwillets/mpvc/"; ref = "master"; };
    installPhase = ''PREFIX="$out" extras/mpvc-installer install'';
  };

  pb = stdenv.mkDerivation rec {
    pname = "pb";
    version = "1.0.0";
    meta.description = "a nice pastebin script";

    src = builtins.fetchGit {url = "https://github.com/syntax-samurai/pb/"; ref = "master"; };
    installPhase = ''install -D -m +rx ./pb $out/bin/pb'';
  };


  xst-git = (pkgs.xst.overrideAttrs(old: {
    # src = /home/neeasade/code/xst;
    src = builtins.fetchGit {url = "https://github.com/neeasade/xst"; ref = "master"; };
  }));

  lemonbar = (pkgs.lemonbar-xft.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/neeasade/bar"; ref = "thicc"; };
  }));

  colort-git = (pkgs.colort.overrideAttrs(old: {
    src = builtins.fetchGit { url = "https://github.com/neeasade/colort"; ref = "master"; };
  }));

  txth = (pkgs.txtw.overrideAttrs(old: {
    src = builtins.fetchGit { url = "https://github.com/neeasade/txth"; ref = "master"; };
  }));

  bspwm-git = (pkgs.bspwm.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/neeasade/bspwm"; ref = "master";};
  }));

  pfetch-neeasade = (edge.pfetch.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/neeasade/pfetch"; ref = "neeasade"; };
  }));

  qutebrowser-git = (pkgs.qutebrowser.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/qutebrowser/qutebrowser"; ref = "master"; };
  }));

  wmutils-opt-git = (pkgs.wmutils-opt.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ [ pkgs.xorg.xcbutil ];
    src = builtins.fetchGit {url = "https://github.com/wmutils/opt"; ref = "master"; };
  }));

  wmutils-core-git = (pkgs.wmutils-core.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/wmutils/core"; ref = "master"; };
  }));

  neeasade-opt = (pkgs.wmutils-opt.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ [ pkgs.xorg.xcbutil ];
    src = builtins.fetchGit {url = "https://github.com/neeasade/opt"; ref = "master"; };
  }));

  xdo-git = (pkgs.xdo.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/baskerville/xdo"; ref = "master"; };
  }));
}
