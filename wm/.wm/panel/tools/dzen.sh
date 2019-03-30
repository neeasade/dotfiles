#!/usr/bin/env bash
# dzen dropdown functions

# This function echos dzen dimensions at the panel centered to click
# along with options from current theme(font)
dzen_options() {
    p_bg_normal="$(colort -t $p_bg_normal)"
    p_fg_normal="$(colort -t $p_fg_normal)"
    p_bg_active="$(colort -t $p_bg_active)"

    longestLine=$(printf '%s\n' "${content[@]}" | sed 's/\^ca([^)]*)//;s/\^ca()//' | wc -L)
    font_name="$(echo $p_font_main | sed 's/-.*//')"
    font_size="$(echo $p_font_main | sed 's/.*-//')"
    width="$(txtw -f "$font_name" -s $font_size a)"
    width="$(echo $width \* $longestLine | bc)"
    width=$((width + 20))

    gapped=$(iif "[ ! $(bspc config window_gap) -le 0 ]")
    p_gap=(iif $gapped $p_gap 0)

    eval $(xdotool getmouselocation --shell)

    #X=$((X+$(bspc query -T -m $mon | jq .rectangle.x)))

    # slide x over by half width, then fit to conflicting monitor edges
    X=$((X-(width/2)))

    # left
    monx=$(bspc query -T -m $mon | jq .rectangle.x)
    [ "$X" -lt "$((monx+p_gap))" ] && X=$((monx+p_gap))
    # right
    monwidth=$(bspc query -T -m $mon | jq .rectangle.width)
    [ "$((X+width))" -gt "$((monx+monwidth+p_gap))" ] && X=$((monx+monwidth-width-p_gap))


    miny=$(bspc query -T -m | jq .rectangle.y)
    if [ "$p_position" = "top" ]; then
        miny=$((miny+p_height+p_gap))
        Y=$miny
    else
        # todo here: use txth w/ line count to calculate where y should be.
        miny=$((miny+p_height+p_gap))
        Y=$miny
    fi

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

    for file in $(eval printf $(elisp '(ns/make-lines (mapcar (fn (concat "\"" (s-replace (~ "") "~/" <>) "\"")) (seq-take recentf-list 5)))')); do
        content+=(" ^ca(1, elisp 'find-file "$file"' $sdw) `icon_dzen file` $file ^ca()")
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
    content+=("load theme")
    for theme in $(theme list); do
        content+=("^ca(1, nohup theme load \"$theme\" & pkill dzen) $theme ^ca()")
    done
}

dzen_circe() {
    content+=("Circe DMs")
    for bufname in $(eval printf $(elisp '(ns/make-lines (ns/circe-unread-query-buffers))')); do
        content+=("^ca(1, nohup ejump $bufname & pkill dzen) $bufname ^ca()")
    done
}

dzen_github() {
    content+=("Github Notifications")
    IFS=$'\n'

    declare -A issueMap

    for input in $( jq -r '.[] | [.subject.title, .subject.url, .repository.name] |join("^")' < "/tmp/gh_notify"); do

        # breaks if issue title contains a ^
	      title="$(echo $input | cut -d '^' -f 1)"
	      url="$(echo $input | cut -d '^' -f 2)"
        url="$(echo $url | tr -d \")"
	      repo="$(echo $input | cut -d '^' -f 3)"

        #[[ -z "${issue[$repo]}" ]] && issuemap[$repo]=
        issueMap[$repo]+=$'\n'"^ca(1, github_nav \"$url\" & pkill dzen) $title ^ca()"
    done

    for repo in "${!issueMap[@]}"; do
        content+=("*** $repo ***")
        for entry in "${issueMap[$repo]}"; do
            content+=($entry)
        done
    done

    #content+=()
    IFS=
}

# This script is ment to be called with the desired function suffix.
# eg: ./dzen.sh mpd, to call dzen_mpd()
[ -z $1 ] && exit 1

# These will get filled by the function called.
content=()
width=
length=

# Call the function.
dzen_$1

# Print the content array to dzen_options
printf "%s\n" "${content[@]}" | eval dzen2 `dzen_options $1` &
