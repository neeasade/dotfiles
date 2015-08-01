#!/bin/sh
# A script to determine title form for different window modes in bspwm
# dependencies: xtitle
# Neeasade
# Arguments: The name of the monitor in bspc to watch over

# behavior:
# output a parsable format similar to bspc control for a separate script to interprete.
# if active window is floating or in a tiled workspace, just output title for active window.
# if active window is in monocle mode, list all the windows in active workspace with windowIDs associated.
# if ALWAYS_TAB is set, always list all windows in the active workspace.

# The one argument will be the monitor that this is for.
CUR_MON=$1;

###OPTIONS###
# Always tabbed visible windows option:
ALWAYS_TAB=false;
# Maxium window title length
maxWinNameLen=20;
# The delimiter to separate window sections
WIN_DELIM="\\";
# The delimiter to separate the window name from the window in a window section.
WIN_ID_DELIM="//";

update() {
    # Current monitor's shown desktop
    CUR_MON_DESK=$( bspc query --monitor ^$CUR_MON -T | grep " - \*" | grep -oE "[0-9]/[^ ]+" );

    # If current window is not in this desktop, no need to update.
    if [ -z "$(bspc query -W -d "$CUR_MON_DESK" | grep "$WIN_SOURCE" )" ]; then
        return;
    fi

    if [ "$ALWAYS_TAB" = true ]; then
        for i in $(bspc query -W -d "$CUR_MON_DESK"); do
            [[ "$i" = "$WIN_SOURCE" ]] && status="A" || status="X";
            winName $i $status;
        done
    else
        # get tiling status of focused desktop on that monitor
        CUR_MON_TILED=$( bspc query -d $CUR_MON_DESK -T | grep "T - \*");

        if [ -z "$CUR_MON_TILED" ]; then CUR_MON_TILED=false; else CUR_MON_TILED=true; fi;

        # Is this the currently active monitor?
        if [ "$(bspc query -m focused -M)" -eq "$CUR_MON" ]; then IS_ACT_MON=true; else IS_ACT_MON=false; fi;
        if [ "$CUR_MON_TILED" = true ]; then
            winName $WIN_SOURCE X;
        elif [ "$CUR_MON_TILED" = false ]; then
            FLOAT_STATUS=$(bspc query -W -w focused.floating);
            if [ ! -z $FLOAT_STATUS ]; then
                winName $WIN_SOURCE A;
            else
                for i in $(bspc query -W -d $CUR_MON_DESK); do
                   [[ "$i" = "$WIN_SOURCE" ]] && status="A" || status="X";
                   winName $i $status;
                done
            fi;
        fi;
    fi
}

# print the name of the window based on id, by using xtitle
# will be prefixed with a 'A' or 'X' depending on if it's the 'active' window for this monitor
winName() {
#    [[ "$1" = "$WIN_SOURCE" ]] && echo -n "A" || echo -n "X";
    winName="$2$(xtitle -t $maxWinNameLen "$1")";

    echo -n "$winName$WIN_ID_DELIM$1$WIN_DELIM";
}

WIN_SOURCE="$(bspc query -H -m "$CUR_MON" | tail -n 1 | grep -oE "[0-9]x.+")"
echo "T$(update)"

bspc control --subscribe window | while read line; do
   WIN_SOURCE="$(echo $line | grep -oE "[0-9]x.+")"
   WINDOWS="T$(update)"
   [ ! "$WINDOWS" = "T" ] && echo "$WINDOWS"
done
