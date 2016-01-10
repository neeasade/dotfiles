#!/bin/sh
# use wmutils to pulse colors based around current bspwm color
# modified from the wmutils contrib repo.

FREQ=0.1

COLORS="$(bspc config active_border_color | cut -c2- )"
LAST_COLOR="$COLORS"

for i in 1 1 1 1 -1 -1 -1 -1 -1 -1 -1 -1 1 1 1; do
    LAST_COLOR=$(colort $i "$LAST_COLOR")
    COLORS="$COLORS $LAST_COLOR"
done

while :; do
    for c in $COLORS; do
        CUR=$(bspc query -N -n)
        test "`wattr wh $CUR`" != "`wattr wh $(lsw -r)`" && chwb -s 2 -c "$c" $CUR
        sleep $FREQ
    done
done
