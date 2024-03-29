{pkgs, shared, lib, ...}:

let
  # to see latest version:
  # https://factorio.com/get-download/stable/headless/linux64
  version = "1.1.94";
  game = "Ilium";
in {
  services.factorio = {
    enable = true;
    description = game;
    game-name = game;
    saveName = game; # nb: save game location is /var/lib/factorio/saves/<saveName>.zip

    # admins = lib.splitString "\n" (builtins.readFile (shared.home + "factorio.admins"));
    admins = [ ];
    game-password = builtins.readFile (shared.home + "factorio.pass");

    mods =
      let
        inherit (pkgs) lib;

        modDir = /home/neeasade/factorio-mods;

        # couldn't get this to work:
        # modDir = builtins.toPath (shared.home + "factorio-mods");

        modList = lib.pipe modDir [
          builtins.readDir
          (lib.filterAttrs (k: v: v == "regular"))
          (lib.mapAttrsToList (k: v: k))
          (builtins.filter (lib.hasSuffix ".zip"))
        ];

        modToDrv = modFileName:
          pkgs.runCommand "copy-factorio-mods" {} ''
          mkdir $out
          cp ${modDir + "/${modFileName}"} $out/${modFileName}
        ''
          // { deps = []; };
      in
        builtins.map modToDrv modList;

    nonBlockingSaving = true;
    openFirewall = true;
    loadLatestSave = true;

    extraSettings = {
      # cf https://github.com/wube/factorio-data/blob/master/server-settings.example.json
      only_admins_can_pause_the_game = false;
      # 10 hours of saves @ 20min intervals
      autosave_slots = 30;
    };

    package = pkgs.factorio-headless.overrideAttrs (_: {
      inherit version;
      src = pkgs.fetchurl {
        url = "https://factorio.com/get-download/${version}/headless/linux64";
        name = "factorio-headless-${version}.tar.xz";
        sha256 = "96289caf52d1c7b15eb95be54104c69d41752b345c6fdc6b984a7cdb06425a2a";
      };
    });
  };
}
