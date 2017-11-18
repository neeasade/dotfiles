{ config, pkgs, stable, rolling, neeasade, ...}:

{
  environment.systemPackages = with stable; [
      gcc autoconf automake gnumake cmake

      (python36.withPackages(ps: with ps; [
        # setuptools
        virtualenv
        django
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
