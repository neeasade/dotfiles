{ fetchFromGitHub, wmutils-opt, xcbutil }:

wmutils-opt.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [ xcbutil ];

  src = fetchFromGitHub {
    owner  = "neeasade";
    repo   = "opt";
    rev    = "f48cc90ff839c7b61bda85ed3845981001056060";
    sha256 = "1rjd0clqqdcw5x5bdxqjh25dbg0bfadwp2i81s3p8m4f49x90jqy";
  };
})
