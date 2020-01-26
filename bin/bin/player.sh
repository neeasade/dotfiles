#!/usr/bin/env dash
# I want one toggle for all my media

dbusplaying=false
dbuspresent=false
if status=$(playerctl status 2>/dev/null); then
  dbusplaying=true

  dbuspresent=true

  if [ ! "$status" = "Playing" ]; then
    # paused or stopped
    dbusplaying=false
  fi

  if [ -z "$(playerctl metadata)" ]; then
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
    # chrome xesam:album
    # chrome xesam:artist              Vapor Memory
    # chrome xesam:title               haircuts for men : когда зло господствует
    metadata=$(playerctl metadata)
    artist=$(echo "$metadata" | awk '/xesam:artist/{$1=$2=""; print $0}')
    title=$(echo "$metadata" | awk '/xesam:title/{$1=$2=""; print $0}')

    for char in \| \- \:; do
      if [ $(echo "$title" | awk -F${char} '{print NF-1}') -eq 1 ]; then
        artist=$(echo "$title" | awk -F${char} '{print $1}')
        title=$(echo "$title" | awk -F${char} '{print $2}')
        break
      fi
    done

    title=$(echo "$title" | awk '{$1=$1;print}')
    artist=$(echo "$artist" | awk '{$1=$1;print}')
    echo "$title - $artist"
    exit 0
  fi
fi

if [ -z "$*" ]; then
  set -- status
fi

# echo $target "$@"
$target "$@"
