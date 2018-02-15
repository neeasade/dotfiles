{ xdo, fetchLatestGit }:

xdo.overrideAttrs(old: {
  src = fetchLatestGit { url = "https://github.com/baskerville/xdo"; };
})
