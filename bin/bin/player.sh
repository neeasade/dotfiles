#!/usr/bin/env bash
# I want one toggle for all my media
# this script needs some more love

dbusplaying=false
dbuspresent=false
if status=$(playerctl -a status 2>/dev/null); then
  dbusplaying=true
  dbuspresent=true

  if ! echo "$status" | grep -q "Playing"; then
    # paused or stopped
    dbusplaying=false
  fi

  if [ -z "$(playerctl metadata 2>/dev/null)" ]; then
    dbuspresent=false
  fi
fi

mpcplaying=$(iif 'mpc | grep -q playing')
mpvcplaying=$(iif 'pgrep mpv > /dev/null && mpvc | grep -q playing')

if ! $mpvcplaying && ! $mpcplaying && ! $dbusplaying; then
  pgrep mpv >/dev/null && target='mpvc' || target='mpc'
  $dbuspresent && target='playerctl'
else
  $mpvcplaying && target='mpvc'
  $mpcplaying  && target='mpc'
  $dbusplaying && target='playerctl'
fi

if [ "$target" = "playerctl" ]; then
  if [ "$*" = "toggle" ]; then
    set -- play-pause
  elif [ "$1" = "-f" ]; then
    fstring=$2
    # from playerctl(1) format strings
    for i in title album artist status volume position playerName; do
      fstring=${fstring/\%${i}%/{{${i}\}\}}
    done
    set -- metadata -f "$fstring"
  fi
fi

if [ -z "$*" ]; then
  if [ "$target" = "playerctl" ]; then
    set -- -a status
  else
    set -- status
  fi
fi

# terrible
if [ -z "$GET" ]; then
  # echo $target "$@"
  $target "$@"
else
  echo $target
fi
