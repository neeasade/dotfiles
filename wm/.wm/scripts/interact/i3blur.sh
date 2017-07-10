#!/usr/bin/env bash

scrot 'tmp.png' -e 'convert -blur 0x3 $f ./lockbg.png'

# place in center of primary monitor.
read x y height width <<< $(bspc query -T -m  | jq  '.rectangle.x, .rectangle.y, .rectangle.height, .rectangle.width' | tr '\n' ' ')
region="${width}x${height}+${x}+${y}"
echo $region

convert -gravity center -region $region -composite lockbg.png ~/.wm/walls/lock.png lockfinal.png

i3lock -u -i lockfinal.png
rm lockfinal.png lockbg.png tmp.png
