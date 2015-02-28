#
# A script to determine title form for tabbed vs not tabbed in bspwm
# dependencies: xtitle
# Neeasade
#

#The one argument will be the monitor that this is for.
CUR_MON=$1;

###OPTIONS###
#Maxium window title length
maxWinNameLen=30;
#The delimiter to separate window sections
WIN_DELIM=">>";
#The delimiter to separate the window name from the window in a window section.
WIN_ID_DELIM="//";
#The interval, in seconds, to check if an update is needed and then call update()
WIN_REFRESH_DELAY=0.1;

update() {
    #Current monitor's shown desktop
    CUR_MON_DESK=$( bspc query --monitor ^$CUR_MON -T | grep " - \*" | grep -oE "[0-9]/i+" );

    #get tiling status of focused desktop on that monitor
    CUR_MON_TILED=$( bspc query -d $CUR_MON_DESK -T | grep "T - \*");

    if [ -z "$CUR_MON_TILED" ]; then export CUR_MON_TILED=false; else export CUR_MON_TILED=true; fi;

    #Is this the currently active monitor?
    if [ "$(bspc query -m focused -M)" -eq "$CUR_MON" ]; then export IS_ACT_MON=true; else export IS_ACT_MON=false; fi;

    #define an 'active' window source to use for this desktop based on weather or not it's the focused monitor.
    if [ "$IS_ACT_MON" = true ]; then
        export WIN_SOURCE="$(bspc query -W -w focused)";
    else
        #Last history object on this monitor should contain last active window.
        export WIN_SOURCE="$( bspc query -H -d $CUR_MON_DESK | tail -n 1 | grep -oE "[0-9]x.+" )";
    fi

    if [ "$CUR_MON_TILED" = true ]; then
        winName $WIN_SOURCE;
    elif [ "$CUR_MON_TILED" = false ]; then
        FLOAT_STATUS=$(bspc query -W -w focused.floating);
        if [ ! -z $FLOAT_STATUS ]; then
            winName $WIN_SOURCE;
        else
            for i in $(bspc query -W -d $CUR_MON_DESK); do
               winName $i;
            done
        fi;
    fi;
}

#print the name of the window based on id, by using xtitle
#will be prefixed with a 'A' or 'X' depending on if it's the 'active' window for this monitor
winName() {
    echo -n "$WIN_DELIM";

    if [ "$1" = "$WIN_SOURCE" ]; then
        echo -n "A";
    else
        echo -n "X";
    fi;

    winName="$(xtitle -t $maxWinNameLen $1)";

    echo -n "$winName";
    echo -n "$WIN_ID_DELIM";
    echo -n "$1";
}

while :; do
    CUR_MON_DESK=$( bspc query --monitor ^$CUR_MON -T | grep " - \*" | grep -oE "[0-9]/i+" );
    #Update ACT_WIN
    CUR_WIN="$( bspc query -H -d $CUR_MON_DESK | tail -n 1 )";
    if [ "$ACT_WIN" != "$CUR_WIN" ]; then
        ACT_WIN=$CUR_WIN;
        title="T$(update)";
        echo $title;
    fi
   sleep $WIN_REFRESH_DELAY;
done
