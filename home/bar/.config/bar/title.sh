#
# A script to determine title form for tabbed vs not tabbed in bspwm
# Neeasade

#The one argument will be the monitor that this is for.
CUR_MON=$1;

WIN_DELIM=">>";

update() {
#Current monitors shown desktop
CUR_MON_DESK=$( bspc query --monitor ^$CUR_MON -T | grep " - \*" | grep -oE "[0-9]/i+" );

#get tiling status of focused desktop on that monitor
CUR_MON_TILED=$( bspc query -d $CUR_MON_DESK -T | grep "T - \*");

if [ -z "$CUR_MON_TILED" ]; then export CUR_MON_TILED=false; else export CUR_MON_TILED=true; fi;

#Is this the currently active monitor?
if [ "$(bspc query -m focused -M)" -eq "$CUR_MON" ]; then export IS_ACT_MON=true; else export IS_ACT_MON=false; fi;

#echo Active desktop: $CUR_MON_DESK
#echo Tiling status: $CUR_MON_TILED
#echo IsActive: $IS_ACT_MON

#In the below, return is the same as 'echo', and will be used in panel_bar script.

#If the current desktop is the active one, determine if we are tiling
#   if we are tiling, just return xtitle.
#   if we are are not tiling, determine if active window is floating
#       if floating, return xtitle
#       if not floating, return titles separated by ':', prefixing active with A

#If the current desktop is not the active one, determine if tiling.
#   if tiling, return title of last active window there.
#   if not tiling, determine if last active window was floating
#       if not floating, return titles separated by ':', prefixing active with A
#       if floating return just active title of last

#define an 'active' window source to use for this desktop based on weather or not it's the focused monitor.
if [ "$IS_ACT_MON" = true ]; then
    export WIN_SOURCE="$(bspc query -W -w focused)";
else
    #Last history object on this monitor should contain last active window.
    export WIN_SOURCE="$( bspc query -H -d $CUR_MON_DESK | tail -n 1 | grep -oE "[0-9]x.+" )";
fi

if [ "$CUR_MON_TILED" = true ]; then
    winName $WIN_SOURCE
elif [ "$CUR_MON_TILED" = false ]; then
    #If current window is floating, return xtitle.
    FLOAT_STATUS=$(bspc query -W -w focused.floating);
    if [ ! -z $FLOAT_STATUS ]; then
        winName $WIN_SOURCE
    else
        for i in $(bspc query -W -d $CUR_MON_DESK); do
           winName $i
        done
    fi;
fi;
}

#print the name of the window based on id, by greping xprop
winName() {
    echo -n "$WIN_DELIM";
    if [ "$1" = "$WIN_SOURCE" ]; then
        echo -n "A";
    else
        echo -n "X";
    fi;

    echo -n "$( xprop -id $1 | grep "WM_NAME(UTF" | grep -oE "\".*\"" | tr -d "\"" )";
    echo -n "//";
    echo -n "$1";
}

while :; do
    title="T$(update)";
    echo $title #| rev | cut -c 3- | rev
    sleep 0.2
done
