#!/usr/bin/env dash
# use wmutils to pulse colors based around current bspwm color
# modified from the wmutils contrib repo.
# depends on bspwm and colort

hash chwb 2>/dev/null || { echo >&2 "$0 requires wmutils."; exit 1;  }

FREQ=0.07
NORMAL_COLOR="$(bspc config normal_border_color | cut -c2- )"

COLORS="$(bspc config active_border_color | cut -c2- )"
LAST_COLOR="$COLORS"

colort -c "$LAST_COLOR" && set="20 20 20 20 20 20 -20 -20 -20 -20 -20 -20" || \
                           set="-20 -20 -20 -20 -20 -20 20 20 20 20 20 20"

for i in $set; do
    NEW_COLOR="$LAST_COLOR"
    LAST_COLOR=$(colort -l $i "$LAST_COLOR")
    COLORS="$COLORS $LAST_COLOR"
done

while :; do
    for c in $COLORS; do
        CUR=$(bspc query -N -n)
        LAST=$CUR
        [ ! "$CUR" = "$LAST" ] && chwb -c "$NORMAL_COLOR" $LAST
        chwb -c "$c" $CUR
        sleep $FREQ
    done
done
