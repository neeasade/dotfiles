{ fetchFromGitHub, txtw, fetchLatestGit }:

txtw.overrideAttrs(old: {
  src = fetchLatestGit { url = "https://github.com/neeasade/txth"; };
})
