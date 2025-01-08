{ edge, pkgs, ... }: with pkgs;
rec {
  hl2350 = callPackage ./HLL2350DW {};

  proton-ge-custom = stdenv.mkDerivation rec {
    pname = "proton-ge-custom";
    version = "GE-Proton8-11";

    src = fetchurl {
      url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
      sha512 = "0bb0359922436b81096bf00c85453587095396e8a2ecdb5d41eabc17784277459c1db312bb52339f292395cc5fbe4cebe6db5dd32eb9748829097078a16138d1";
    };

    installPhase = ''mkdir -p $out; mv * $out/ '';
  };

  mpvc-git = stdenv.mkDerivation rec {
    pname = "mpvc";
    version = "1.5-version";

    # src = builtins.fetchGit {url = "https://github.com/lwillets/mpvc/"; ref = "master"; };

    src = fetchFromGitHub {
      owner = "lwilletts";
      repo = "mpvc";
      rev = version;
      sha256 = "sha256-kodHy9DV/bih3Fpy0H64m30/+TdvQ26cxyWJizG1cL0=";
    };

    installPhase = ''PREFIX="$out" extras/mpvc-installer install'';
  };

  pb = stdenv.mkDerivation rec {
    pname = "pb";
    version = "1.0.0";
    meta.description = "a nice pastebin script";

    src = builtins.fetchGit {url = "https://github.com/syntax-samurai/pb/"; ref = "main"; };
    installPhase = ''install -D -m +rx ./pb $out/bin/pb'';
  };

  colort-git = (pkgs.colort.overrideAttrs(old: {
    src = builtins.fetchGit { url = "https://github.com/neeasade/colort"; ref = "master"; };
  }));

  bspwm-git = (pkgs.bspwm.overrideAttrs(old: {
    src = builtins.fetchGit {url = "https://github.com/neeasade/bspwm"; ref = "master";};
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

  # TODO oomox
  # gdk_pixbuf
  # glib.dev
  # gtk-engine-murrine
  # gtk3
  # sassc
}
