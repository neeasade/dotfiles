#!/bin/sh
# swap between fake fullscreen modes as monocle mode
# this is so we can enjoy fake padding in fullscreen things

gap=$(theme getval b_window_gap)

if [ "$(bspc query -T -d | jq -r .layout)" = "monocle" ]; then
    if [ $(bspc config window_gap) -gt 0 ]; then
	bspc config window_gap 0
    else
	bspc config window_gap $gap
	bspc desktop -l next
	theme refresh bg
    fi
else
    bspc desktop -l next
    hsetroot -solid "#$(theme getval background)"
fi
