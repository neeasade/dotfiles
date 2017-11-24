{ stdenv, fetchLatestGit, pkgconfig, gtk2 }:

stdenv.mkDerivation rec {
  name = "gtkrc-reload-v${version}";
  version = "1.0";

  src = fetchLatestGit { url = "https://github.com/neeasade/gtkrc-reload"; };

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
