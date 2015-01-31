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
       echo "NO BATT"
    fi
}

volume() {
    amixer get Master | sed -n 's/^.*\[\([0-9]\+%\).*$/\1/p'
}

memused() {
    read t f <<< `grep -E 'Mem(Total|Free)' /proc/meminfo |awk '{print $2}'`
    bc <<< "scale=2; 100 - $f / $t * 100" | cut -d. -f1
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
        echo "$int ✔" || echo "$int ✖"
}

nowplaying() {
    cur=`mpc current | grep -oE '/([^/]+)\.'`
    # this line allow to choose whether the output will scroll or not
    test -n "$cur" && cat <<< $cur || echo "stopped"
}

# This loop will fill a buffer with our infos, and output it to stdout.
delim=//
while :; do
    buf="S" buf="${buf} $(battery) $delim "
    buf="${buf} ⭯ $(nowplaying) $delim "
    buf="${buf} $(network) $delim "
    buf="${buf} ⭦ $(memused)%% $delim "
    buf="${buf} ◂⋑ $(volume)%% $delim "
    buf="${buf} ⭧ $(clock)"

    echo $buf
    # use `nowplaying scroll` to get a scrolling output!
    sleep 1 # The HUD will be updated every second
done

