{ stdenv, fetchFromGitHub, pkgconfig, gtk2 }:

stdenv.mkDerivation rec {
  name = "gtkrc-reload-v${version}";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "neeasade";
    repo = "gtkrc-reload";
    rev = "v${version}";
    sha256 = "1vdcqvyfl0id9f0mwhgzxnfnv9k0ijq39ym31qgmc3yql91ghgi0";
  };

  buildInputs = [ pkgconfig gtk2 ];

   installPhase = ''
     make install PREFIX=$out
   '';

  meta = with stdenv.lib; {
    description = "Utility for reloading gtk2 program themes";
    inherit (src.meta) homepage;
    license = licenses.gpl3;
    platforms = platforms.all;
  };
}
