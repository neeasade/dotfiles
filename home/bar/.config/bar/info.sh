#!/bin/sh
# info.sh
# Output information with formatted background colors in lemonbar format
# This script can take arguments for what bar information to display(meant to be the names of the functions)


# clickable area aliases
AC='%{A:'           # start click area
AB=':}'             # end click area cmd
AE='%{A}'           # end click area

clock() {
    date '+%b%e,%l:%M '
}

battery() {
    BATC=/sys/class/power_supply/BAT0/capacity
    BATS=/sys/class/power_supply/BAT0/status
    if [ -f $BATC ]; then
        test "`cat $BATS`" = "Charging" && echo -n '+' || echo -n '-'
        sed -n p $BATC
    else
        #no battery information found.
        echo '+100'
    fi
}

volume() {
    echo "vol $(amixer get Master | sed -n 's/^.*\[\([0-9]\+%\).*$/\1/p')"
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
    echo -n "⇅ "
    ping -c 1 8.8.8.8 >/dev/null 2>&1 &&
        echo "✔" || echo "✖"
}

mpd() {
    #TODO: Add clickable controls next to songs, trim song name.
    cur_song=$(mpc current)

    if [ -z "$cur_song" ]; then
        echo "Stopped"
    else
        echo $cur_song
    fi
}

yaourtUpdates() {
    #TODO: make this clickable to open up terminal that prompts for updating.
    updates=$(eval yaourt -Qu | wc --lines)
    echo up $updates
}

# The {E} bar command below provides a slant from this fork: http://github.com/neeasade/bar
delim=" ${pBGS1}%{E${pSLANT}}$(printf %${pSLANT}s)${pFG} "
delim2=" ${pBGS2}%{E${pSLANT}}$(printf %${pSLANT}s)${pFG} "

#determine what to display based on arguments, unless there are none, then display all.
while :; do
    buf="S"
    if [ -z "$*" ];then
        buf="${buf}${delim2}$(yaourtUpdates)"
        buf="${buf}${delim}$(mpd)"
        buf="${buf}${delim2}$(battery)"
        buf="${buf}${delim}$(network)"
        buf="${buf}${delim2}$(volume)"
        buf="${buf}${delim}$(clock)"
    else
        cur_delim="$delim2"
        for arg in "$@"; do
            buf="${buf}${cur_delim}$($arg)"
            [ "$cur_delim" = "$delim" ] && cur_delim="$delim2" || cur_delim="$delim"
        done
    fi

    echo "$buf$pBG"
    sleep 1 # The HUD will be updated every second
done

