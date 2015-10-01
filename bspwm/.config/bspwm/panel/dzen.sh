#!/usr/bin/env bash
# dzen dropdown functions

# This function echos dzen dimensions at the panel centered to click
# along with options from current theme(font)
dzen_options()
{
    pBG="$(cut -c4- <<< $pBG)"
    pFG="$(cut -c4- <<< $pFG)"
    pBGActiveTab="$(cut -c4- <<< $pBGActiveTab)"

    eval $(xdotool getmouselocation --shell)

    X=$(expr $X - $(expr $width / 2))

    #handle offsets
    xextra=`bspc query -T -d | head -n 1 | grep -oE "[^+ ]+" | sed -n '4 p'`
    Y=`expr $PANEL_HEIGHT + $PANEL_GAP + $xextra`
    xedge=`bspc query -T -d | head -n 1 | grep -oE "[^x ]+" | sed -n '2 p'`
    xoffset=`bspc query -T -d | head -n 1 | grep -oE "[^+ ]+" | sed -n '3 p'`
    if [[ `expr $xoffset + $xedge - $X` -lt $width ]];then
        X=`expr $xoffset + $xedge - $width - $PANEL_GAP`
    fi
    if [[ `expr $xoffset + $PANEL_GAP` -gt $X ]];then
        X=`expr $xoffset + $PANEL_GAP`
    fi

    # set the alignment(default to center)
    [[ -z $align ]] && align=c

    echo -l $length -fg \"#$pFG\" -bg \"#$pBG\" -fn \"$PANEL_FONT_MAIN\" -x \"$X\" -y \"$Y\" -w \"$width\" -p 5 -sa $align -e \'onstart=uncollapse\;button1=menuprint,exit\;button3=exit\;button4=scrollup\;button5=scrolldown\' -m v
}

# Songs next to current song in playlist(click to play them)
dzen_mpd()
{
    width=300
    length=10

    lines=5
    format="%position%;%title%"
    content+=("Playlist")
    IFS=$'\n'
    for song in `mpc playlist -f "$format" | grep -$lines "$(mpc current -f %position%)" | head -n $length` ; do
        position=${song%%";"*}
        songname=${song#*";"}
        content+=(" ^ca(1, mpc play $position ) ${songname} ^ca() ")
    done
}

# Common folders and recent files, as well as some actions.
dzen_menu()
{
    width=200
    length=14
    align=l
    icon() {
        echo -n "^fn($PANEL_FONT_ICON)"
        echo -n -e "\u$1"
        echo -n "^fn()"
    }

    # stupid dzen workaround.
    sdw="& pkill dzen"
    content+=("+")

    content+=(" Common folders")
    for folder in Downloads Documents Images; do
        content+=("   ^ca(1, pcmanfm "$HOME/$folder" $sdw) `icon f07b` $folder ^ca()")
    done

    content+=(" Recently edited ")
    # Get recent files from .nviminfo by vim marks.
    for file in $(cat ~/.nviminfo | grep -A 10 "File marks" | grep -oE "~.+" | uniq | head -n 5); do
        fullfile="$(echo $file | sed "s/~/\/home\/$USER/" )"
        content+=("   ^ca(1, termite -e 'vim $fullfile' $sdw) `icon f15c` $file ^ca()")
    done

    content+=(" Actions")
    content+=("   ^ca(1, i3blur.sh $sdw) `icon f023` Lock ^ca()")
    content+=("   ^ca(1, bspc quit) `icon f08b` Logoff ^ca()")
    content+=("   ^ca(1, shutdown $sdw) `icon f011` Shutdown ^ca()")
}

# Calendar from cal with current date highlighted
dzen_cal()
{
    # handle the fonts I use - monospace is better here.
    PANEL_FONT_MAIN=`sed 's/Droid Sans/Droid Sans Mono/; s/:style=Bold//; s/Dejavu Sans/Dejavu Sans Mono/' <<< "$PANEL_FONT_MAIN"`

    # dynamic width using txtw
    font_size="${PANEL_FONT_MAIN#*-}"
    font_name="${PANEL_FONT_MAIN%-*}"

    # txtw seems to be off by one..
    font_size=`expr $font_size + 1`

    width=$(txtw -f "xft:$font_name:style=Regular" -s $font_size "$(cal | sed -n "4 p" | sed "s/ /a/g")" )
    length=7

    TODAY=$(expr `date +'%d'` + 0)
    MONTH=`date +'%m'`
    YEAR=`date +'%Y'`

    # highlight current date
    # currently broken
    content+=$(
    cal | sed -re "s/^(.*[A-Za-z][A-Za-z]*.*)$/^fg(#$pFG)^bg(#$pBG)\1/;
    s/(^|[ ])($TODAY)($|[ ])/\1^bg(#$pBGActiveTab)^fg(#$pBG)\2^fg(#$pFG)^bg(#$pBG)\3/"
    )
}

# Theme switcher
dzen_theme()
{
    width=100
    length=`ls ~/.config/bspwm/themes | wc -l`
    (( length -= 1 ))

    content+=("+")
    for theme in `ls ~/.config/bspwm/themes | grep -v base | sed s/.bspwm_theme// `; do
        content+=("^ca(1, nohup ltheme $theme & pkill dzen) $theme ^ca()")
    done
}

# This script is ment to be called with the desired function suffix.
# eg: ./dzen.sh mpd, to call dzen_mpd()
[ -z $1 ] && exit 1

# Get panel, font, colors values.
. ~/.bspwm_theme

# These will get filled by the function called.
content=()
width=
length=

# Call the function.
dzen_$1

# Print the content array to dzen_options
printf "%s\n" "${content[@]}" | eval dzen2 `dzen_options` &
