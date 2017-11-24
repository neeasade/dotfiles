{ fetchLatestGit, wmutils-opt, xcbutil }:

wmutils-opt.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [ xcbutil ];
  src = fetchLatestGit { url = "https://github.com/neeasade/opt"; };
})
