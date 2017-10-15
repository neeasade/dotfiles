{ config, pkgs, stable, rolling, neeasade, ...}:

{
  environment.systemPackages = with stable; [
      gcc autoconf automake gnumake cmake

      (python35.withPackages(ps: with ps; [
        setuptools
      ]))

      nodejs
      go
      rustc rustracer rustfmt
      jdk8 maven gradle
      clojure leiningen boot
      ruby

      # haskell
      ghc
      guile

      # libs
      zlib

      # devops
      docker
  ];
}
