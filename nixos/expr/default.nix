{ pkgs ? import <nixpkgs> {}, ... }: with pkgs;

let
  # ref: http://chriswarbo.net/projects/nixos/useful_hacks.html

  sanitiseName = lib.stringAsChars (c: if lib.elem c (lib.lowerChars ++ lib.upperChars)
                                      then c
                                      else "");

  fetchGitHashless = args: stdenv.lib.overrideDerivation
    # Use a dummy hash, to appease fetchgit's assertions
    (fetchgit (args // { sha256 = hashString "sha256" args.url; }))

    # Remove the hash-checking
    (old: {
      outputHash     = null;
      outputHashAlgo = null;
      outputHashMode = null;
      sha256         = null;
    });

  # Get the commit ID for the given ref in the given repo
  latestGitCommit = { url, ref ? "HEAD" }:
    runCommand "repo-${sanitiseName ref}-${sanitiseName url}"
      {
        # Avoids caching. This is a cheap operation and needs to be up-to-date
        version = toString builtins.currentTime;

        # Required for SSL
        GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

        buildInputs = [ git gnused ];
      }
      ''
        REV=$(git ls-remote "${url}" "${ref}") || exit 1

        printf '"%s"' $(echo "$REV"        |
                        head -n1           |
                        sed -e 's/\s.*//g' ) > "$out"
      '';

  fetchLatestGit = { url, ref ? "HEAD" }@args:
    with { rev = import (latestGitCommit { inherit url ref; }); };
    fetchGitHashless (removeAttrs (args // { inherit rev; }) [ "ref" ]);
in
rec {
  bspwm-git = callPackage ./bspwm { inherit fetchLatestGit; };
  wmutils-opt-git = callPackage ./wmutils-opt {  inherit fetchLatestGit; };
  neeasade-opt = callPackage ./neeasade-opt {  inherit fetchLatestGit; };
  gtkrc-reload = callPackage ./gtkrc-reload {  inherit fetchLatestGit; };
  bevelbar = callPackage ./bevelbar { };
}
