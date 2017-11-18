{ fetchFromGitHub, wmutils-opt, xcbutil }:

wmutils-opt.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [ xcbutil ];

  src = fetchFromGitHub {
    owner  = "wmutils";
    repo   = "opt";
    rev    = "00ea524ff7ddedd0627c229acf4a9d0a0d72a711";
    sha256 = "1kp6aql2ibvvpccjf4cij0pdlgxssdzi6pk8dwia30xwg6r09nws";
  };
})
