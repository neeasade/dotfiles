{
  pkgs,
  shared,
  lib,
  ...
}: let
  # to see latest version:
  # https://factorio.com/get-download/stable/headless/linux64
  version = "1.1.87";

in {
  services.factorio = {
    enable = true;
    description = "VIKINGS";
    game-name = "VIKINGS";

    admins = lib.splitString "\n" (builtins.readFile "/home/neeasade/sync/main/notes/factorio.admins");
    game-password = builtins.readFile "/home/neeasade/sync/main/notes/factorio.pass";
    nonBlockingSaving = true;
    openFirewall = true;
    loadLatestSave = true;

    # note: save game location is /var/lib/factorio/saves
    saveName = "planet_of_the_bass";

    package = pkgs.factorio-headless.overrideAttrs (_: {
      inherit version;
      src = pkgs.fetchurl {
        url = "https://factorio.com/get-download/${version}/headless/linux64";
        name = "factorio-headless-${version}.tar.xz";
        sha256 = "60b3884b6dad1f4c7b30b7ef2b63619ff4a3204ac7fd894cf09d382b349857cc";
      };
    });
  };
}
