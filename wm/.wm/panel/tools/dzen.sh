#!/usr/bin/env bash
# dzen dropdown functions

# This function echos dzen dimensions at the panel centered to click
# along with options from current theme(font)
dzen_options() {
    #if [[ ! "$1" = "cal" ]]; then
        p_bg_normal="$(colort -t $p_bg_normal)"
        p_fg_normal="$(colort -t $p_fg_normal)"
        p_bg_active="$(colort -t $p_bg_active)"
    #fi

    eval $(xdotool getmouselocation --shell)

    #X=$((X+$(bspc query -T -m $mon | jq .rectangle.x)))

    miny=$(bspc query -T -m | jq .rectangle.y)
    miny=$((miny+p_height+p_gap))
    Y=$miny

    length="${#content[@]}"

    # todo: make this dynamic
    width=200

    # set the alignment(default to center)
    [[ -z $align ]] && align=c
    echo -l $((${#content[@]}-1)) -fg \"#$p_fg_normal\" -bg \"#$p_bg_normal\" -fn \"$p_font_main\" -x \"$X\" -y \"$Y\" -w \"$width\" -p 5 -sa $align -e \'onstart=uncollapse\;button1=menuprint,exit\;button3=exit\;button4=scrollup\;button5=scrolldown\' -m v

}

# Songs next to current song in playlist(click to play them)
dzen_mpd() {
    lines=5
    format="%position%;%title%"
    content+=("Playlist")
    IFS=$'\n'
    for song in `mpc playlist -f "$format" | grep -$lines "$(mpc current -f %position%)" | head -n 10` ; do
        position=${song%%";"*}
        songname=${song#*";"}
        content+=(" ^ca(1, mpc play $position ) ${songname} ^ca() ")
    done
}

# Common folders and recent files, as well as some actions.
dzen_menu() {
    set +e
    notify-send asdf

    align=l
    icon_dzen() {
        if  ! grep -q "Siji" <<< "$p_font_icon"; then echo -n "   "; fi
        echo -n "^fn($p_font_icon)$(icon $1)^fn()"
    }

    # stupid dzen workaround.
    sdw="& pkill dzen"
    content+=("+")

    content+=(" Common folders")
    for folder in Downloads Documents Images; do
        content+=(" ^ca(1, pcmanfm "$HOME/$folder" $sdw) `icon_dzen folder` $folder ^ca()")
    done

    content+=(" Recently edited ")
    # Get recent files from .viminfo by vim marks.
    for file in $(cat ~/.viminfo | grep -A 10 "File marks" | grep -oE "~.+" | uniq | head -n 5); do
        fullfile="$(echo $file | sed "s/~/\/home\/$USER/" )"
        content+=(" ^ca(1, termite -e 'vim $fullfile' $sdw) `icon_dzen file` $file ^ca()")
    done

    content+=(" Actions")
    content+=(" ^ca(1, i3blur.sh $sdw) `icon_dzen lock` Lock ^ca()")
    content+=(" ^ca(1, bspc quit) `icon_dzen logoff` Logoff ^ca()")
    content+=(" ^ca(1, shutdown $sdw) `icon_dzen shutdown` Shutdown ^ca()")
}

# Calendar from cal with current date highlighted
dzen_cal() {
    # handle the fonts I use - monospace is better here.
    p_font_main=`sed 's/Droid Sans/Droid Sans Mono/; s/:style=Bold//; s/Dejavu Sans/Dejavu Sans Mono/' <<< "$p_font_main"`

    # dynamic width using txtw:
    font_size="${p_font_main#*-}"
    font_name="${p_font_main%-*}"

    # txtw seems to be off by one..
    font_size=`expr $font_size + 1`

    width=$(txtw -f "xft:$font_name:style=Regular" -s $font_size "$(cal | sed -n "4 p" | sed "s/ /a/g")" )

    TODAY=$(expr `date +'%d'` + 0)

    #$p_bg_normal="$(colort )"
    #pFG="$(cut -c4- <<< $pFG)"
    #pBGActiveTab="$(cut -c4- <<< $pBGActiveTab)"

    # highlight current date
    #for input in "$(cal | sed -re "s/(^|[ ])( $TODAY )($|[ ])/\1^bg(#$pBGActiveTab)^fg(#$p)\2^fg(#$pFG)^bg(#$pBG)\3/")";do
        #content+=("$input")
    #done
    IFS=$'\n'
    for i in $(cal); do
        content+=("$i")
    done
    IFS=

}

# Theme switcher
dzen_theme() {
    content+=("+")
    for theme in `ls ~/.wm/themes | grep -v base | sed s/.bspwm_theme// `; do
        content+=("^ca(1, nohup ltheme $theme & pkill dzen) $theme ^ca()")
    done
}

dzen_github() {
    content+=("Notifications")
    IFS=$'\n'
    for input in $(jq '.[].subject.title, .[].subject.url' < "/tmp/gh_notify" | tr '\n' ','); do
	      title="$(echo $input | cut -d ',' -f 1)"
	      url="$(echo $input | cut -d ',' -f 2)"
        url="$(echo $url | tr -d \")"

        link="$(curl -H "Authorization: token $(pass github/token)" $url | jq '.html_url')"

        content+=("^ca(1, nohup $BROWSER $link & pkill dzen) $title ^ca()")
    done
    IFS=
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
printf "%s\n" "${content[@]}" | eval dzen2 `dzen_options $1` &
