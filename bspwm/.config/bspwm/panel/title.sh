#!/usr/bin/env bash
# A script to determine title form for different window modes in bspwm
# dependencies: xtitle
# Neeasade
# Arguments: The name of the monitor in bspc to watch over

# behavior:
# output a parsable format similar to bspc control for a separate script to interprete.
# if active window is floating or in a tiled workspace, just output title for active window.
# if active window is in monocle mode, list all the windows in active workspace with windowIDs associated.
# if always_tab is set, always list all windows in the active workspace.

# The one argument will be the monitor that this is for.
CUR_MON=$1;

###OPTIONS###
# Always tabbed visible windows option:
always_tab=false;
# Maxium window title length
maxWinNameLen=25;
# The delimiter to separate window sections
win_delim="\\";
# The delimiter to separate the window name from the window in a window section.
win_id_delim="//";

update() {
    # Current monitor's shown desktop
    CUR_MON_DESK=$(bspc query -D --desktop "$CUR_MON:focused");

    # If current window is not in this desktop, no need to update.
    [[ -z "$(bspc query -W -d "$CUR_MON_DESK" | grep "$win_source" )" ]] && return

    if [[ "$always_tab" = true ]]; then
        for i in $(bspc query -W -d "$CUR_MON_DESK"); do
            [[ "$i" = "$win_source" ]] && status="A" || status="X";
            winName $i $status;
        done
    else
        if [[ `bspc query -d $CUR_MON_DESK -T | jshon -e layout -u` = tiled ]]; then
            winName $win_source X;
        else
            FLOAT_STATUS=$(bspc query -W -w focused.floating);
            if [[ ! -z $FLOAT_STATUS ]]; then
                winName $win_source A;
            else
                for i in $(bspc query -W -d $CUR_MON_DESK); do
                   [[ "$i" = "$win_source" ]] && status="A" || status="X";
                   winName $i $status;
                done
            fi;
        fi;
    fi
}

# print the name of the window based on id, by using xtitle
# will be prefixed with a 'A' or 'X' depending on if it's the 'active' window for this monitor
winName() {
    winName="$2$(xtitle -t $maxWinNameLen "$1")";

    echo -n "$winName$win_id_delim$1$win_delim";
}

win_source="$(bspc query -H -m "$CUR_MON" | tail -n 1 | grep -oE "[0-9]x.+")"
echo "T$(update)"

bspc control --subscribe window | while read line; do
   if grep $(bspc query -D --desktop "$CUR_MON:focused") <<< "$line"; then
      if grep unmanage <<< "$line"; then
         echo "T "
      else
         win_source="$(echo $line | grep -oE "[0-9]x.+")"
         WINDOWS="T$(update)"
         [ ! "$WINDOWS" = "T" ] && echo "$WINDOWS"
      fi
   fi
done
