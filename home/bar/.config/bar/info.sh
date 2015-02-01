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
    cur=`mpc current | grep -oE '/([^/]+)\.'`
    # this line allow to choose whether the output will scroll or not
    test -n "$cur" && cat <<< $cur || echo "stopped"
}

# This loop will fill a buffer with our infos, and output it to stdout.
delim=%{F#FF404040}⮂%{B#FF404040}%{F#FFFFFFFF}
delim2=%{F#FF505050}⮂%{B#FF505050}%{F#FFFFFFFF}
while :; do
    buf="S $delim2"
    buf="${buf} ⭫ $(battery) $delim "
    buf="${buf} ⭯ $(nowplaying) $delim2 "
    buf="${buf} ⇅ $(network) $delim "
    buf="${buf} ◂⋑ $(volume)%% $delim2 "
    buf="${buf} ⭧ $(clock)"

    echo $buf
    # use `nowplaying scroll` to get a scrolling output!
    sleep 1 # The HUD will be updated every second
done

