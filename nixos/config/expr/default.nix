{ pkgs ? import <nixpkgs> {}, edge ? import <nixpkgs>, ... }: with pkgs;
  let
    # I'm not sure of the correct form here rn
    placeholder = "temp";
  in
    rec {
      gtkrc-reload = callPackage ./gtkrc-reload {};

      foliate = stdenv.mkDerivation rec {
        pname = "foliate";
        version = "1.5.3";

        # Fetch this from gnome mirror if/when available there instead!
        src = fetchFromGitHub {
          owner = "johnfactotum";
          repo = pname;
          rev = version;
          sha256 = "1bjlk9n1j34yx3bvzl95mpb56m2fjc5xcd6yks96pwfyfvjnbp93";
        };

        nativeBuildInputs = [
          meson
          ninja
          pkgconfig
          gettext
          python3
          desktop-file-utils
          wrapGAppsHook
          hicolor-icon-theme
        ];

        buildInputs = [
          glib
          gtk3
          gjs
          webkitgtk
          gsettings-desktop-schemas
          gobject-introspection
          libarchive
          # TODO: Add once packaged, unclear how language packages best handled
          # hyphen
          dict # dictd for offline dictionary support
        ];

        doCheck = true;

        postPatch = ''
    chmod +x build-aux/meson/postinstall.py
    patchShebangs build-aux/meson/postinstall.py
  '';

        # Kludge so gjs can find resources by using the unwrapped name
        # Improvements/alternatives welcome, but this seems to work for now :/.
        # See: https://github.com/NixOS/nixpkgs/issues/31168#issuecomment-341793501
        postInstall = ''
    sed -e $'2iimports.package._findEffectiveEntryPointName = () => \'com.github.johnfactotum.Foliate\' ' \
      -i $out/bin/com.github.johnfactotum.Foliate
  '';

        meta = with stdenv.lib; {
          description = "Simple and modern GTK eBook reader";
          homepage = "https://johnfactotum.github.io/foliate/";
          license = licenses.gpl3Plus;
          maintainers = with maintainers; [ dtzWill ];
        };
      };

      skroll = stdenv.mkDerivation rec {
        name = "skroll";
        src = builtins.fetchGit {url = "https://github.com/z3bra/skroll"; ref = "master"; };
        nativeBuildInputs = [ gcc ];
        installPhase = "make install PREFIX=$out";
      };

      oomox = stdenv.mkDerivation rec {
        # src = builtins.fetchGit {
        name = "oomox";
        src = builtins.fetchGit {
          "url" = "https://github.com/themix-project/oomox/";
          # "rev" = "483d011551458472738bc58014bcfa8d05e49cdd";
          "ref" = "master";
          # "sha256" = "1flkly48ck9s82gyifxrm542fbdjinsrd1pkyxj39bdx3x6hrfpj";
          # "fetchSubmodules" = false;
        };

        nativeBuildInputs = [
          gdk_pixbuf
          glib.dev
          gtk-engine-murrine
          gtk3
          sassc
        ];

        installPhase = "make install PREFIX=$out";
      };

      # note: won't work for ARM oof
      babashka = stdenv.mkDerivation rec {
        name = "babashka";
        # reminder: nix-prefetch-url <url>
        src = (fetchurl {
          url = "https://github.com/borkdude/babashka/releases/download/v0.1.3/babashka-0.1.3-linux-amd64.zip";
          sha256 = "0nldq063a1sfk0qnkd37dpw8jq43p4divn4j4qiif6dy1qz9xdcq";
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

      pb-git = stdenv.mkDerivation rec {
        pname = "pb";
        version = "1.0.0";

        src = builtins.fetchGit {url = "https://github.com/syntax-samurai/pb/"; ref = "master"; };
        installPhase = ''install -D -m +rx ./pb $out/bin/pb'';

        meta = {
          description = "a nice pastebin script";
          platforms   = stdenv.lib.platforms.unix;
        };
      };

      xst-git = (pkgs.xst.overrideAttrs(old: {
        src = builtins.fetchGit {url = "https://github.com/neeasade/xst"; ref = "master"; };
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
        src = builtins.fetchGit {url = "https://github.com/neeasade/bspwm"; ref = "master"; };
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
