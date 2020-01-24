{ stdenv, pkgconfig, gdk_pixbuf, glib.dev, gtk-engine-murrine, gtk3, sassc}:
# { stdenv, pkgconfig, gtk2 }:


stdenv.mkDerivation rec {
  name = "oomox-v${version}";
  version = "1.12.5.3";

  src = builtins.fetchTarball ;
  # "https://github.com/themix-project/oomox/archive/${version}.tar.gz"

  # buildInputs = [ pkgconfig gtk2 ];
  buildInputs = [ gdk_pixbuf glib.dev gtk-engine-murrine gtk3 sassc ];


   installPhase = ''
     make install PREFIX=$out/
   '';

  meta = with stdenv.lib; {
    description = "Utility for reloading gtk2 program themes";
    inherit (src.meta) homepage;
    license = licenses.gpl3;
    platforms = platforms.all;
  };
}
