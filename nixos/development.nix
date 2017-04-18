{ config, pkgs, stable, rolling, neeasade, ...}:

{
  environment.systemPackages = with stable; [
      gcc autoconf automake gnumake cmake
      python
      nodejs
      go
      rustc rustracer rustfmt
      jdk8 jre8 maven gradle
      clojure leiningen
      ruby
      # haskell
      ghc
      # todo: dotnet
  ];
}
