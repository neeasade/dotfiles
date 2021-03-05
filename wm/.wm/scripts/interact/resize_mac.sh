#!/bin/sh

. $HOME/.sh.d/environment

percent=5
dir=$1

case $dir in
  west)  dir=left;   fallDir=right;  targetProp=w; op="min"; sign=-;;
  east)  dir=right;  fallDir=left;   targetProp=w; op="max"; sign=+;;
  north) dir=top;    fallDir=bottom; targetProp=h; op="min"; sign=-;;
  south) dir=bottom; fallDir=top;    targetProp=h; op="max"; sign=+;;
  *) exit 1;;
esac

moveArgs="$sign$(echo "$percent/100*$(yaboi query display | jq .frame.$targetProp)" | bc -l | awk -F. '{print $1}')"
[ $targetProp = "h" ] && moveArgs="0:$moveArgs" || moveArgs="$moveArgs:0"

if yaboi query window floating; then
  yabai -m window --resize $dir:$moveArgs
  exit 0
fi

yabai -m window --resize $dir:$moveArgs || \
  yabai -m window --resize $fallDir:$moveArgs

