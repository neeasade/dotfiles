#!/bin/sh
# info.sh
# Output general information with formatted background colors in bar-aint-recursive format
# TODO: If Verbose_bar has a value in bar/profile, will output more information

# default location for a battery capacity

clock() {
    date '+%b%e,%l:%M'
}

battery() {
    # if we reach here is assumed this computer has a battery.
    BATC=/sys/class/power_supply/BAT0/capacity
    BATS=/sys/class/power_supply/BAT0/status
    if [ -f $BATC ]; then
        test "`cat $BATS`" = "Charging" && echo -n '+' || echo -n '-'
        sed -n p $BATC
    else
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

# The {E} bar command below provides a slant from this fork: http://github.com/neeasade/bar
delim=" ${pBGS1}%{E${pSLANT}}$(printf %${pSLANT}s)${pFG} "
delim2=" ${pBGS2}%{E${pSLANT}}$(printf %${pSLANT}s)${pFG} "

#determine what to display based on arguments, unless there are none, then display all.
while :; do
    if [ -z "$*" ];then
        buf="S"
        buf="${buf}${delim2}$(battery)"
        buf="${buf}${delim}$(network)"
        buf="${buf}${delim2}$(volume)"
        buf="${buf}${delim}$(clock)"
    else
        cur_delim="$delim2"
        buf="S"
        for arg in "$*"; do
            buf="${buf} $cur_delim $($arg) "
            [ "$cur_delim" = "$delim" ] && cur_delim="$delim2" || cur_delim="$delim"
        done
    fi

    echo "$buf"
    sleep 1 # The HUD will be updated every second
done

