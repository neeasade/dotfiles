{ fetchFromGitHub, bspwm, fetchLatestGit }:

bspwm.overrideAttrs(old: {
  src = fetchLatestGit { url = "https://github.com/baskerville/bspwm"; };
})
