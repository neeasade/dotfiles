{ fetchFromGitHub, qutebrowser, fetchLatestGit }:

# this will break if any deps change.
qutebrowser.overrideAttrs(old: {
  src = fetchLatestGit { url = "https://github.com/qutebrowser/qutebrowser"; };
})

