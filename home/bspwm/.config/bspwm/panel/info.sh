#!/bin/sh
# info.sh
# Output information with formatted background colors in lemonbar format
# This script can take arguments for what bar information to display(meant to be the names of the functions)


# Alternating separators for display items
delim=" ${pBGS1}%{E${pSLANT}}$(printf %${pSLANT}s)${pFG} "
delim2=" ${pBGS2}%{E${pSLANT}}$(printf %${pSLANT}s)${pFG} "

# clickable area aliases
AC='%{A:'           # start click area
AB=':}'             # end click area cmd
AE='%{A}'           # end click area

icon() {
    echo -n -e "${cIcon}\u$1 ${cContent}"
}

weather() {
    icon f0c2
    weatherURL='http://www.accuweather.com/en/us/manhattan-ks/66502/weather-forecast/328848'
    wget -q -O- "$weatherURL" | awk -F\' '/acm_RecentLocationsCarousel\.push/{print $12"°F" }'| head -1
}

clock() {
    icon f073
    date '+%b%e,%l:%M'
}

mail() {
    # todo: this
    icon f0e0
    echo '0'
}

battery() {
    BATC=/sys/class/power_supply/BAT0/capacity
    BATS=/sys/class/power_supply/BAT0/status
    icon f0e7
    if [ -f $BATC ]; then
        [ "`cat $BATS`" = "Charging" ] && echo -n '+' || echo -n '-'
        cat $BATC
    else
        #no battery information found.
        echo '+100'
    fi
}

volume() {
    display="$(icon f028) $(amixer get Master | sed -n 's/^.*\[\([0-9]\+%\).*$/\1/p')"
    command='urxvtc -e sh -c "alsamixer"'
    echo ${AC}$command${AB}$display${AE}
}

network() {
    read lo int1 int2 <<< `ip link | sed -n 's/^[0-9]: \(.*\):.*$/\1/p'`
    if iwconfig $int1 >/dev/null 2>&1; then
        wifi=$int1
        eth0=$int2
    else
        wifi=$int2
        eth0=$int1
    fi
    ip link show $eth0 | grep 'state UP' >/dev/null && int=$eth0 ||int=$wifi
    icon f0ac
    ping -W 1 -c 1 8.8.8.8 >/dev/null 2>&1 &&
        echo -e '\uf00c' || echo -e '\uf00d'
}

mpd() {
    cur_song=$(mpc current | cut -c1-30)

    icon f025
    if [ -z "$cur_song" ]; then
        echo "Stopped"
    else
        paused=$(mpc | grep paused)
        [ -z "$paused" ] && echo "${AC}mpc pause${AB} $cur_song${AE}" ||
                            echo "${AC}mpc play${AB} $cur_song${AE}"
    fi
}

yaourtUpdates() {
    updates=$(eval yaourt -Qu | wc --lines)
    command='urxvtc -e sh -c "yaourt -Syua"'
    echo ${AC}$command${AB}$(icon f062)$updates${AE}
}

themeSwitch() {
    # ghetto
    cur_theme=$(cat ~/.bspwm_theme | grep THEME_NAME | cut -c12-)
    case $cur_theme in
        ashes) next_theme=pyonium ;;
        pyonium) next_theme=jellybean ;;
        jellybean) next_theme=ashes ;;
    esac
    command="ltheme $next_theme"
    icon f01e
    echo ${AC}$command${AB}$cur_theme${AE}
}

#determine what to display based on arguments, unless there are none, then display all.
while :; do
    buf="S"
    if [ -z "$*" ];then
        buf="${buf}${delim}$(themeSwitch)"
        buf="${buf}${delim2}$(mpd)"
        buf="${buf}${delim}$(mail)"
        buf="${buf}${delim2}$(yaourtUpdates)"
        buf="${buf}${delim}$(battery)"
        buf="${buf}${delim2}$(network)"
        buf="${buf}${delim}$(volume)"
        buf="${buf}${delim2}$(weather)"
        buf="${buf}${delim}$(clock)"
    else
        cur_delim="$delim2"
        for arg in "$@"; do
            buf="${buf}${cur_delim}$($arg)"
            [ "$cur_delim" = "$delim" ] && cur_delim="$delim2" || cur_delim="$delim"
        done
    fi

    echo "$buf $pBG"
    sleep 1 # The HUD will be updated every second
done
