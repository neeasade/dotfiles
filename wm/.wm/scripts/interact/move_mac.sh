#!/bin/sh
# stub for now
# the other rel window command to checkout is:
# yabai -m window --warp $dir
dir=$1

. ~/.sh.d/environment

case $dir in
  west)  fdim=w; tdim=height; sign=-;;
  east)  fdim=w; tdim=height;;
  north) fdim=h; tdim=width; sign=-;;
  south) fdim=h; tdim=width;;
esac

if ! yaboi query window floating; then
  if ! yabai -m window --swap $dir; then
    yabai -m window --toggle split
    # if going north or west, swap opposite dir? assumes split is like a rotation
    # ...this whole thing would be so much easier if there were parents/a tree
    # if [ ! -z "$sign" ]; then
    #   yabai -m window --toggle split
    # fi
  fi
  exit 0
fi

# floating move
percent=5
moveArgs="$sign$(echo "$percent/100*$(yaboi query display | jq .frame.$fdim)" | bc -l | awk -F. '{print $1}')"
if [ $fdim = "h" ]; then
  moveArgs="0:$moveArgs"
else
  moveArgs="$moveArgs:0"
fi
yabai -m window --move rel:$moveArgs
