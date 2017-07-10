#!/usr/bin/env dash

# target is picked based on which might be playing.
mpc | grep -q playing && mpcplaying=true || mpcplaying=false
mpvc | grep -q playing && mpvcplaying=true || mpvcplaying=false

if ! $mpvcplaying && ! $mpcplaying; then
  mpvc >/dev/null && target='mpvc' || target='mpc'
else
  $mpvcplaying && target='mpvc'
  $mpcplaying && target='mpc'
fi

$target $*
