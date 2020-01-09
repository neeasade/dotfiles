#!/bin/sh
# make some wid floating, center, focused
# or, que up the next wid to be the above, via a rule.

wid=$1

if [ -z "$wid" ]; then
    wid=$(bspc query -N -n)
fi

# note: maybe consolidate the dmenu_options logic and this together
dim() {
    bspc query -T -m | jq .rectangle.$1
}

width=$(echo .66 \* $(dim width) | bc | sed 's/\..*//')
height=$(echo .33 \* $(dim width) | bc | sed 's/\..*//')
x=$(( ($(dim width) - width) / 2 ))
y=$(( ($(dim height) - height) / 4 ))

if [ "$wid" = "-r" ]; then
  bspc rule -a \* -o state=floating rectangle=${width}x${height}+${x}+${y}
  exit 0
fi

bspc node $wid -t floating
xdotool windowmove $wid $x $y
xdotool windowsize $wid $width $height
bspc node $wid -f

