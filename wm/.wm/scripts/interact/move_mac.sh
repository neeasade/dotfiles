#!/bin/sh
# stub for now
# the other rel window command to checkout is:
# yabai -m window --warp $dir
dir=$1

. ~/.sh.d/environment

if ! yaboi query window floating; then
  yabai -m window --swap $dir
  exit 0
fi


case $dir in
  west)  fdim=w; tdim=height; sign=-;;
  east)  fdim=w; tdim=height;;
  north) fdim=h; tdim=width; sign=-;;
  south) fdim=h; tdim=width;;
esac

percent=5
moveArgs="$sign$(echo "$percent/100*$(yaboi query display | jq .frame.$fdim)" | bc -l | awk -F. '{print $1}')"
[ $fdim = "h" ] && moveArgs="0:$moveArgs" || moveArgs="$moveArgs:0"
yabai -m window --move rel:$moveArgs
