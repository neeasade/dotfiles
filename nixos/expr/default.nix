{ pkgs ? import <nixpkgs> {} }: with pkgs;

rec {

bspwm-git = callPackage ./bspwm { };
wmutils-opt-git = callPackage ./wmutils-opt { };
neeasade-opt = callPackage ./neeasade-opt { };
gtkrc-reload = callPackage ./gtkrc-reload { };

}
