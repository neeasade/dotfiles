{ fetchFromGitHub, bspwm }:

bspwm.overrideAttrs(old: {
  src = fetchFromGitHub {
    owner  = "baskerville";
    repo   = "bspwm";
    rev    = "1bb6386fe07ed99f32a24c4f71e0d5fe00419bea";
    sha256 = "0bc98kl8zamli0ixkka6hk157a4njnxsfq1iji21wanz046nljs6";
  };
})
