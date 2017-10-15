#!/usr/bin/env dash

# target is picked based on which might be playing.
mpc  | grep -q playing && mpcplaying=true  || mpcplaying=false

if pgrep mpv > /dev/null && mpvc | grep -q playing; then
    mpvcplaying=true
else
    mpvcplaying=false
fi

if ! $mpvcplaying && ! $mpcplaying; then
  pgrep mpv > /dev/null && target='mpvc' || target='mpc'
else
  $mpvcplaying && target='mpvc'
  $mpcplaying  && target='mpc'
fi

$target "$@"
