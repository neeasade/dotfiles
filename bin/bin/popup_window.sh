#!/usr/bin/env bash
# make some wid floating, center, focused

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

bspc node $wid -t floating

# resize
xdotool windowmove $wid $x $y
xdotool windowsize $wid $width $height

bspc node $wid -f
