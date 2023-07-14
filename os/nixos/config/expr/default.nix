{ pkgs ? import <nixpkgs> {},
  edge ? import <nixpkgs>, ... }: with pkgs;
  let
    # todo: no let
    _ = "_";
    # edge_graal =
  in
    rec {
      gtkrc-reload = callPackage ./gtkrc-reload {};
      hl2350 = callPackage ./HLL2350DW {};

      clj-find-usage = callPackage ./clj-find-usage {};

     # note: won't work for ARM oof
      babashka = stdenv.mkDerivation rec {
        name = "babashka";

        # NB: regenerate with nix-prefetch-url <zip url>
        src = (fetchurl {
          url = "https://github.com/babashka/babashka/releases/download/v0.2.10/babashka-0.2.10-linux-amd64.zip";
          sha256 = "0s24jzmh7qdcc9kdfd4lhcdh9ihgvb32wy8rbh9xdmg2izvwnrpa";
        });

        unpackPhase = "unzip $src";

        # note: the chmod is only needed when using direct path to local executable
        patchPhase = ''
            patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" ./bb
            new_rpath=$(echo "$NIX_LDFLAGS" | tr ' ' $'\n' | grep "^-L" | sed -E 's/^-L/:/' | tr -d $'\n')
            patchelf --set-rpath "$new_rpath" ./bb
        '';

        dontBuild = true;

        installPhase = ''
            mkdir -p $out/bin
            cp bb $out/bin
        '';

        nativeBuildInputs = (with pkgs; [gcc-unwrapped.lib zlib unzip]);
      };


      proton-ge-custom = stdenv.mkDerivation rec {
        pname = "proton-ge-custom";
        version = "GE-Proton8-6";

        src = fetchurl {
          url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/${version}/${version}.tar.gz";
          sha512 = "e92dcfe60b24c552a10e3218d42ef19006f388840b0df9363a6884b90356e34def6dc315862979f9192de6b27ab14725080b583043602cccd9daa498888207db";
        };

        installPhase = ''
            mkdir -p $out
            mv * $out/
        '';
        };

      pb-git = stdenv.mkDerivation rec {
        pname = "pb";
        version = "1.0.0";

        src = builtins.fetchGit {url = "https://github.com/syntax-samurai/pb/"; ref = "master"; };
        installPhase = ''install -D -m +rx ./pb $out/bin/pb'';

        meta = {
          description = "a nice pastebin script";
        };
      };

      xst-git = (pkgs.xst.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/xst"; ref = "master"; };
        # src = /home/neeasade/git/xst;
      }));

      lemonbar= (pkgs.lemonbar-xft.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/bar"; ref = "thicc"; };
      }));

      colort-git = (pkgs.colort.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/colort"; ref = "master"; };
      }));

      mpvc-git = (pkgs.mpvc.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/lwilletts/mpvc"; ref = "master"; };
      }));

      txth = (pkgs.txtw.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/txth"; ref = "master"; };
      }));

      dmenu = (pkgs.dmenu.overrideAttrs(old: {
        patches = [];
        src = builtins.fetchGit {url = "https://github.com/neeasade/dmenu"; ref = "master"; };
      }));

      bspwm-git = (pkgs.bspwm.overrideAttrs(old: {
        src = builtins.fetchGit {
	    url = "https://github.com/neeasade/bspwm";
	    ref = "master";
	};
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

      wmutils-core-git = (pkgs.wmutils-opt.overrideAttrs(old: {
        buildInputs = old.buildInputs ++ [ pkgs.xorg.xcbutil pkgs.xcb-util-cursor ];
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
