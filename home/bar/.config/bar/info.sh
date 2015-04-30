#!/bin/sh
# info.sh
# Output general information with formatted background colors in bar-aint-recursive format
# TODO: If Verbose_bar has a value in bar/profile, will output more information

# default location for a battery capacity
export BATC=/sys/class/power_supply/BAT0/capacity

# clickable area aliases
AC='%{A:'           # start click area
AB=':}'             # end click area cmd
AE='%{A}'           # end click area

clock() {
    date '+%b%e,%l:%M'
}

battery() {
    # if we reach here is assumed this computer has a battery.
    BATS=/sys/class/power_supply/BAT0/status
    test "`cat $BATS`" = "Charging" && echo -n '+' || echo -n '-'
    sed -n p $BATC
}

volume() {
    amixer get Master | sed -n 's/^.*\[\([0-9]\+%\).*$/\1/p'
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

    ping -c 1 8.8.8.8 >/dev/null 2>&1 &&
        echo "✔" || echo "✖"
}

# The {E} bar command below provides a slant from this fork: http://github.com/neeasade/bar
delim="${pBGS1}%{E4}    ${pFG}"
delim2="${pBGS2}%{E4}    ${pFG}"

while :; do
    buf="S$delim2"
    if [ -f $BATC ]; then
        buf="${buf} $(battery) $delim "
    else
        buf="${buf} ⇅ $(network) $delim "
    fi
    buf="${buf} vol $(volume) $delim2 "
    buf="${buf} $(clock)"

    echo "$buf"
    sleep 1 # The HUD will be updated every second
done

