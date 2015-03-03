#!/bin/sh

clock() {
    date '+%H:%M'
}

battery() {
    BATC=/sys/class/power_supply/BAT0/capacity
    BATS=/sys/class/power_supply/BAT0/status

    #Check if there is a battery on the cyrrent computer we are on.
    if [ -f $BATC ]; then
        test "`cat $BATS`" = "Charging" && echo -n '+' || echo -n '-'
        sed -n p $BATC
    else
        #yeah
        echo "+0"
    fi
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

    #int=eth0

    ping -c 1 8.8.8.8 >/dev/null 2>&1 &&
        echo "✔" || echo "✖"
}

nowplaying() {
    cur=`mpc current`
    # this line allow to choose whether the output will scroll or not
    test -n "$cur" && `skroll -n 20 -d0.5 -r` <<< $cur || echo "stopped"
}

# This loop will fill a buffer with our infos, and output it to stdout.
pBGS1="%{B$pS1}"  # bg shade 1
pBGS2="%{B$pS2}"  # bg shade 2

pFGS1="%{F$pS1}"  # fg shade 1
pFGS2="%{F$pS2}"  # fg shade 2

pFG="%{F$pFG}"    # reset fg color

delim=${pFGS1}${pBGS1}${pFG}
delim2=${pFGS2}${pBGS2}${pFG}
while :; do
    buf="S $delim2"
    buf="${buf} ⭫ $(battery) $delim "
    buf="${buf} ⭯ $(nowplaying) $delim2 "
    buf="${buf} ⇅ $(network) $delim "
    buf="${buf} ◂⋑ $(volume)%% $delim2 "
    buf="${buf} ⭧ $(clock)"

    echo $buf
    sleep 1 # The HUD will be updated every second
done

