#!/usr/bin/env bash

act() { playerctl "$@" ;}

playing=$(paste <(playerctl -l) <(playerctl -a status) | grep Playing)
if [ $(wc -l <<< "$playing" ) -gt 1 ]; then
  read -r player _ < <(tail -n 1 <<< "$playing")
  act() { playerctl -p "$player" "$@" ;}
fi

if [ "$*" = "toggle" ]; then
  set -- play-pause
elif [ "$1" = "-f" ]; then
  set -- metadata -f "$2"
fi

act "$@"
