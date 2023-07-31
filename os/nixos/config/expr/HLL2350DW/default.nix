# from https://github.com/NixOS/nixpkgs/issues/53027

{ stdenv, fetchurl, makeWrapper
, cups
, lib
, dpkg
, a2ps, ghostscript, gnugrep, gnused, coreutils, file, perl, which
}:

stdenv.mkDerivation rec {
  name = "hll2350dw-cups-${version}";
  version = "4.0.0-1";

  src = fetchurl {
    url = "https://download.brother.com/welcome/dlf103566/hll2350dwpdrv-${version}.i386.deb";
    sha256 = "0b7hhln105agc3rwpi7cjlx5nf4d2yk9iksahdv3725nnd06lg46";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ cups ghostscript dpkg a2ps ];

  unpackPhase = ":";

  installPhase = ''
    dpkg-deb -x $src $out

    substituteInPlace $out/opt/brother/Printers/HLL2350DW/lpd/lpdfilter \
      --replace /opt "$out/opt" \
      --replace /usr/bin/perl ${perl}/bin/perl \
      --replace "BR_PRT_PATH =~" "BR_PRT_PATH = \"$out\"; #" \
      --replace "PRINTER =~" "PRINTER = \"HLL2350DW\"; #"

    # FIXME : Allow i686 and armv7l variations to be setup instead.
    _PLAT=x86_64
    patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      $out/opt/brother/Printers/HLL2350DW/lpd/$_PLAT/brprintconflsr3
    patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      $out/opt/brother/Printers/HLL2350DW/lpd/$_PLAT/rawtobr3
    ln -s $out/opt/brother/Printers/HLL2350DW/lpd/$_PLAT/brprintconflsr3 $out/opt/brother/Printers/HLL2350DW/lpd/brprintconflsr3
    ln -s $out/opt/brother/Printers/HLL2350DW/lpd/$_PLAT/rawtobr3 $out/opt/brother/Printers/HLL2350DW/lpd/rawtobr3

    for f in \
      $out/opt/brother/Printers/HLL2350DW/cupswrapper/lpdwrapper \
      $out/opt/brother/Printers/HLL2350DW/cupswrapper/paperconfigml2 \
    ; do
      #substituteInPlace $f \
      wrapProgram $f \
        --prefix PATH : ${lib.makeBinPath [
          coreutils ghostscript gnugrep gnused
        ]}
    done

    # Hack suggested by samueldr.
    sed -i"" "s;A4;Letter;g" $out/opt/brother/Printers/HLL2350DW/inf/brHLL2350DWrc

    mkdir -p $out/lib/cups/filter/
    ln -s $out/opt/brother/Printers/HLL2350DW/lpd/lpdfilter $out/lib/cups/filter/brother_lpdwrapper_HLL2350DW

    mkdir -p $out/share/cups/model
    ln -s $out/opt/brother/Printers/HLL2350DW/cupswrapper/brother-HLL2350DW-cups-en.ppd $out/share/cups/model/

    wrapProgram $out/opt/brother/Printers/HLL2350DW/lpd/lpdfilter \
      --prefix PATH ":" ${ lib.makeBinPath [ ghostscript a2ps file gnused gnugrep coreutils which ] }
    '';

  meta = with lib; {
    homepage = "https://www.brother.com/";
    description = "Brother HL-L2350DW combined print driver";
    license = licenses.unfree;
    platforms = [
      "x86_64-linux"
    ];
    downloadPage = "https://support.brother.com/g/b/downloadlist.aspx?c=us&lang=en&prod=hll2350dw_us_eu_as&os=128";
  };
}
