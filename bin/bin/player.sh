#!/usr/bin/env bash

act() {
  playerctl "$@"
}

playing=$(paste <(playerctl -l) <(playerctl -a status) | grep Playing)
if [ $(wc -l <<< "$playing" ) -gt 1 ]; then
  # hopefully mpd
  read -r player _ < <(tail -n 1 <<< "$playing")
  act() {
    playerctl -p "$player" "$@"
  }
fi

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

act "$@"
